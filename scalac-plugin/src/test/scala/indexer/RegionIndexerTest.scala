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
        if (debugTests) {
          Indexer.queryResultAsString(modelName, "select * { ?s ?p ?o }", model) foreach println
          Indexer.queryResultAsString(modelName, query, model) foreach println
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
        abstract class [[AC]]
      """)
  }

  @Test
  def classes_with_body() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "class"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        package a.b.c
        class [[A]] {}
        class [[B_?]] {
          def f = 0
        }
        class [[!!!]] { /* comment*/ }
        class [[`hello world`]] {
          def g = 0
        }
      """)
  }

  @Test
  def objects() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "object"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        package a.b.c
        object [[`x y`]] {
          def g = 0
        }
      """)
  }

  @Test
  def traits() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "trait"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        package a.b.c
        trait [[B]] {}
      """)
  }

  @Test
  def abstract_classes() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "class", "abstract"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        package a.b.c
        abstract class [[AC]] {}
      """)
  }

  @Test
  def packages() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "package"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        package [[a]].[[b]].[[c]]
        class A
      """)
  }

  @Test
  def defs() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "def"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class A {
          def [[meth]] = 0
          def [[meth2]] = {
            def [[meth3]] = {
              def [[meth4]] = 0
              meth4
            }
            meth3
          }
        }
      """)
  }

  @Test
  def vals() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "val"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class A {
          val [[v1]] = 0
          val [[v2]] = {
            val [[v3]] = {
              val [[v4]] = 0
              v4
            }
            v3
          }
        }
      """)
  }

  @Test
  def vars() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "var"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class A {
          var [[v1]] = 0
          var [[v2]] = {
            var [[v3]] = {
              var [[v4]] = 0
              v4
            }
            v3
          }
        }
      """)
  }

  @Test
  def lazy_vals() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "lazy", "val"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class A {
          lazy val [[v1]] = 0
          lazy val [[v2]] = {
            lazy val [[v3]] = {
              lazy val [[v4]] = 0
              v4
            }
            v3
          }
        }
      """)
  }

  @Test
  def private_class_parameters() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "parameter"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class A([[value1]]: Int, [[`second val`]]: String)
      """)
  }

  @Test
  def method_parameters() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "parameter"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def f([[param1]]: Int)([[`p a r a m`]]: String)([[p]]: Int) = 0
        }
      """)
  }

  @Test
  def return_type_at_members() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "typeref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          val a: [[Int]] = 0
          var b: [[Int]] = 0
          def c: [[Int]] = 0
          lazy val d: [[Int]] = 0
        }
      """)
  }

  @Test
  def return_type_at_nested_members() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "typeref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def x: [[Int]] = {
            val a: [[Int]] = {
              val a: [[Int]] = 0
              a
            }
            var b: [[Int]] = {
              var a: [[Int]] = 0
              a
            }
            def c: [[Int]] = {
              def a: [[Int]] = 0
              a
            }
            a
          }
        }
      """)
  }

  @Test
  def return_type_at_nested_lazy_vals() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "typeref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          lazy val a: [[Int]] = {
            lazy val a: [[Int]] = {
              lazy val a: [[Int]] = 0
              a
            }
            a
          }
        }
      """)
  }
}
