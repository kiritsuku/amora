package indexer

import scala.util._

import org.junit.Test

import plugin.TestUtils

class RegionIndexerTest {

  import TestUtils._

  case class Data(varName: String, value: String)

  def ask(modelName: String, rawQuery: String, rawData: (String, String)*): Unit = {
    def findRegions(src: String, prevStart: Int, prevEnd: Int, regions: IndexedSeq[(Int, Int, String)]): IndexedSeq[(Int, Int, String)] = {
      val start = src.indexOf("[[", prevEnd)
      if (start < 0)
        regions
      else {
        val end = src.indexOf("]]", start)
        val len = regions.length
        val prevBrackets = len*4
        findRegions(src, start, end, regions :+ ((start - prevBrackets, end - prevBrackets - "[[".length, src.substring(start+2, end))))
      }
    }
    val dataWithRegions = rawData map {
      case (filename, rawSrc) ⇒
        val regions = findRegions(rawSrc, 0, 0, Vector())
        val src = rawSrc.replaceAll("""\[\[|\]\]""", "")
        (filename, src, regions)
    }

    val data = dataWithRegions.map { case (filename, src, _) ⇒ (filename, src) }
    val expectedRegions = dataWithRegions.flatMap { case (_, _, region) ⇒ region }
    val hierarchyData = convertToHierarchy(data: _*)
    val query = rawQuery.replaceFirst("""\?MODEL\?""", modelName)
    val foundRegions = Indexer.withInMemoryDataset { dataset ⇒
      Indexer.withModel(dataset, modelName) { model ⇒
        hierarchyData foreach {
          case (filename, data) ⇒
            Indexer.add(modelName, filename, data)(model)
        }
        Indexer.withQueryService(modelName, query)(model).map { r ⇒
          import scala.collection.JavaConverters._
          r.asScala.toSeq.map { row ⇒
            val start = row.get("start")
            require(start != null, "No field with name `start` found.")
            val end = row.get("end")
            require(end != null, "No field with name `end` found.")
            val name = row.get("name")
            require(name != null, "No field with name `name` found.")

            (start.asLiteral().getInt, end.asLiteral().getInt, name.toString)
          }.sortBy { case (start, end, _) ⇒ (start, end) }
        }
      }.flatten
    }.flatten

    foundRegions match {
      case Success(foundRegions) ⇒
        foundRegions === expectedRegions

      case Failure(f) ⇒
        throw new RuntimeException("An error happened during the test.", f)
    }
  }

  def modelName = "http://test.model/"

  @Test
  def classes() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "class"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        package a.b.c
        class [[A]]
        class [[B_?]]
        class [[!!!]]
        class [[`hello world`]]
        object O
      """)
  }
}
