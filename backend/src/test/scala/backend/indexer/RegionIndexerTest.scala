package backend
package indexer

import org.junit.Test

import backend.actors.IndexerMessage

class RegionIndexerTest {

  import backend.TestUtils._

  sealed trait Region {
    def len: Int
  }
  case class Range(start: Int, end: Int, str: String) extends Region {
    override def len = "[[]]".length
  }
  case class Offset(offset: Int, str: String) extends Region {
    override def len = "[[!]]".length + str.length
  }

  /**
   * Runs a test against the indexer. `modelName` is the name of the index that
   * shall be used during the test. `rawQuery` is the query that can contain the
   * string `"?MODEL?"`, which is replaced during the test by `modelName`.
   * `rawData` are tuples of the form `(filename, source)`. The sources will be
   * typechecked and indexed and once this is done the query will run against
   * the index.
   *
   * The sources can contain markers that start with `[[` and end with `]]`.
   * These markers are the start and end position of a range, which shall be
   * returned by the query. If the first character after the `[[` marker is a
   * exclamation mark, the range will become an offset region, whose start and
   * end position are the same. Offset regions need to be used when implicit
   * regions need to be tested for their existence (for example implicit return
   * types of methods). The sources are freed of the region markers before they
   * are passed to the typechecker to make it convenient to write tests.
   */
  def ask(modelName: String, rawQuery: String, rawData: (String, String)*): Unit = {
    def findRegions(src: String, prevStart: Int, prevEnd: Int, regions: IndexedSeq[Region]): IndexedSeq[Region] = {
      val start = src.indexOf("[[", prevEnd)
      if (start < 0)
        regions
      else {
        val end = src.indexOf("]]", start)
        val len = regions.map(_.len).sum
        val isOffset = src(start + 2) == '!'
        val range =
          if (isOffset)
            Offset(start - len, src.substring(start + 3, end))
          else
            Range(start - len, end - len - "[[".length, src.substring(start + 2, end))
        findRegions(src, start, end, regions :+ range)
      }
    }
    val dataWithRegions = rawData map {
      case (filename, rawSrc) ⇒
        val regions = findRegions(rawSrc, 0, 0, Vector())
        val src = rawSrc.replaceAll("""\[\[!.*?\]\]|\[\[|\]\]""", "")
        (filename, src, regions)
    }

    val regionOrdering: Region ⇒ (Int, Int, String) = {
      case Range(start, end, text) ⇒ (start, end, text)
      case Offset(offset, text) ⇒ (offset, offset, text)
    }

    val data = dataWithRegions.map { case (filename, src, _) ⇒ (filename, src) }
    val expectedRegions = dataWithRegions.flatMap { case (_, _, region) ⇒ region }.sortBy(regionOrdering)

    val sindexer = new ScalaSourceIndexer(IgnoreLogger)
    val hierarchyData = sindexer.convertToHierarchy(data) match {
      case scala.util.Success(res) ⇒ res
      case scala.util.Failure(f) ⇒ throw f
    }

    val query = rawQuery.replaceFirst("""\?MODEL\?""", modelName)
    val indexer = new Indexer(modelName)
    val dataset = indexer.mkInMemoryDataset
    val foundRegions: Seq[Region] = indexer.writeDataset(dataset) { dataset ⇒
      indexer.withModel(dataset) { model ⇒
        hierarchyData foreach {
          case (filename, data) ⇒
            indexer.addFile(IndexerMessage.File(IndexerMessage.NoOrigin, filename, data))(model)
        }
        if (debugTests) {
          println(indexer.queryResultAsString("select * { ?s ?p ?o }", model))
          println(indexer.queryResultAsString(query, model))
        }
        val r = indexer.withQueryService(model, query)
        import scala.collection.JavaConverters._
        r.asScala.toSeq.map { row ⇒
          val start = row.get("start")
          require(start != null, "No field with name `start` found.")
          val end = row.get("end")
          require(end != null, "No field with name `end` found.")
          val name = row.get("name")
          require(name != null, "No field with name `name` found.")

          if (start.asLiteral.getInt == end.asLiteral().getInt)
            Offset(start.asLiteral().getInt, name.toString())
          else
            Range(start.asLiteral().getInt, end.asLiteral().getInt, name.toString)
        }.sortBy(regionOrdering)
      }
    }

    foundRegions === expectedRegions
  }

  def modelName = "http://test.model/"

  @Test
  def multiple_annotations() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:tpe "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        @[[Ann1]]([[!apply]][[!Class]][[Array]]([[classOf]] [ [[X]] ]))
        @[[Ann2]]
        @[[Ann1]]([[!apply]][[!Class]][[Array]]([[classOf]] [ [[X]] ]))
        class X
        class Ann1(arr: [[Array]][ [[Class]] [_] ]) extends [[scala]].[[annotation]].[[StaticAnnotation]]
        class Ann2 extends [[scala]].[[annotation]].[[StaticAnnotation]]
      """)
  }

  @Test
  def multiple_lambda_decls() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "val"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def f([[i]]: Int ⇒ Int) = i
          f([[v]] ⇒ v)
          f([[v]] ⇒ v)
        }
      """)
  }

  @Test
  def refs_of_lambda_decl() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def [[!Function1]]f([[!Function1]]i: [[Int]] ⇒ [[Int]]) = [[i]]
        }
      """)
  }

  @Test
  def refs_of_function_decl() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def [[!Function1]]f([[!Function1]]i: [[Function1]][ [[Int]], [[Int]] ]) = [[i]]
        }
      """)
  }

  @Test
  def multiple_lambda_refs() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def [[!Function1]]f([[!Function1]]i: [[Int]] ⇒ [[Int]]) = [[i]]
          [[f]]([[!Int]]v ⇒ [[v]])
          [[f]]([[!Int]]value ⇒ [[value]])
        }
      """)
  }

  @Test
  def ref_with_qualifier() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "f1.scala" → """
        package a.b
        import [[d]].[[e]]
        class X {
          val f: [[e]].[[Y]] = null
        }
      """,
      "f2.scala" → """
        package d.e
        class Y
      """)
  }
}
