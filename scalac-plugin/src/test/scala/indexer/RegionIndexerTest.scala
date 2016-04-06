package indexer

import scala.util._

import org.junit.Test

import plugin.TestUtils

class RegionIndexerTest {

  import TestUtils._

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

    val hierarchyData = convertToHierarchy(data: _*)
    val query = rawQuery.replaceFirst("""\?MODEL\?""", modelName)
    val foundRegions: Try[Seq[Region]] = Indexer.withInMemoryDataset { dataset ⇒
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

            if (start.asLiteral.getInt == end.asLiteral().getInt)
              Offset(start.asLiteral().getInt, name.toString())
            else
              Range(start.asLiteral().getInt, end.asLiteral().getInt, name.toString)
          }.sortBy(regionOrdering)
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
        class [[??]]
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
        class [[??]] { /* comment*/ }
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
          [c:attachment "param"] s:name ?name ; c:start ?start ; c:end ?end .
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
          [c:attachment "param"] s:name ?name ; c:start ?start ; c:end ?end .
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
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
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
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def x: [[Int]] = {
            val a: [[Int]] = {
              val a: [[Int]] = 0
              [[a]]
            }
            var b: [[Int]] = {
              var a: [[Int]] = 0
              [[a]]
            }
            def c: [[Int]] = {
              def a: [[Int]] = 0
              [[a]]
            }
            [[a]]
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
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          lazy val a: [[Int]] = {
            lazy val a: [[Int]] = {
              lazy val a: [[Int]] = 0
              [[a]]
            }
            [[a]]
          }
        }
      """)
  }

  @Test
  def member_ref() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          val [[!Int]]a = 0
          def [[!Int]]b = [[a]]
          var [[!Int]]c = [[b]]
          lazy val [[!Int]]d = [[c]]
        }
      """)
  }

  @Test
  def classOf_ref() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          val [[!Class]]a = [[classOf]][ /* Int */ [[Int]] ]
        }
      """)
  }

  @Test
  def refs_of_imports() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        import [[scala]].[[collection]].[[mutable]].[[Buffer]]
        import [[scala]].[[collection]].[[mutable]].[[ListBuffer]]
        class X {
          [[Buffer]]
          [[ListBuffer]]
        }
      """)
  }

  @Test
  def refs_of_rename_imports() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        import [[scala]].[[collection]].[[mutable]].{ [[Buffer]] ⇒ [[B]], [[ListBuffer]] }
        class X {
          [[B]]
          [[ListBuffer]]
        }
      """)
  }

  @Test
  def self_ref_with_fully_qualified_name() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        trait X {
          self: [[scala]].[[collection]].[[mutable]].[[AbstractSet]][ [[java]].[[io]].[[File]] ] ⇒
        }
      """)
  }

  @Test
  def refs_in_if_expr() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          val [[!Boolean]]b1 = true
          val [[!Boolean]]b2 = true
          val [[!Boolean]]b3 = true
          def [[!Boolean]]f = if ([[b1]]) [[b2]] else [[b3]]
        }
      """)
  }

  @Test
  def refs_of_single_method() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          ?def c:attachment "def", "(IF)I" .
          [c:attachment "ref"] c:owner ?def ; s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def f(i: Int) = i
          def [[!Int]]f(i: Int, s: Float) = [[i]]
        }
      """)
  }

  @Test
  def refs_of_parameter() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] c:reference [c:attachment "param"] ; s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def f(i: Int) = {
            [[i]]
          }
        }
      """)
  }

  @Test
  def refs_of_local_value_with_same_name_as_parameter() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] c:reference [c:attachment "param"] ; s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def f(i: Int) = {
            val i = 0
            i
          }
        }
      """)
  }

  @Test
  def refs_to_local_value_when_parameter_of_same_name_exists() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          ?val c:attachment "val" .
          FILTER NOT EXISTS {
            ?val c:attachment "param" .
          }
          [c:attachment "ref"] c:reference ?val ; s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def f(i: Int) = {
            val i = 0
            [[i]]
          }
        }
      """)
  }

  @Test
  def refs_of_type_parameter() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "ref"] c:reference [c:attachment "tparam"] ; s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        trait X[A] {
          def f[B](a: [[A]], b: [[B]]): [[A]]
        }
      """)
  }

  @Test
  def refs_of_type_parameter_without_shadowed_type_parameter_refs() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          # find type parameter
          ?tparam c:owner [c:attachment "trait"] ; c:attachment "tparam" .
          # find references of type parameter
          [c:attachment "ref"] c:reference ?tparam ; s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        trait X[A] {
          def f(a: [[A]], b: [[A]]): [[A]]
          def f[A](a: A): A
        }
      """)
  }

  @Test
  def refs_of_shadowed_type_parameter() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          # find type parameter
          ?tparam c:owner [c:attachment "def", "(Ljava/lang/Object;)Ljava/lang/Object;"] ; c:attachment "tparam" .
          # find references of type parameter
          [c:attachment "ref"] c:reference ?tparam ; s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        trait X[A] {
          def f(a: A, b: A): A
          def f[A](a: [[A]]): [[A]]
        }
      """)
  }

  @Test
  def refs_of_type_parameter_when_parameter_of_same_name_exists() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          # find type parameter
          ?tparam c:owner [c:attachment "def"] ; c:attachment "tparam" .
          # find references of type parameter
          [c:attachment "ref"] c:reference ?tparam ; s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def [[!A]]f[A](A: [[A]]) = {
            A
          }
        }
      """)
  }

  @Test
  def refs_of_type_parameter_when_local_val_decl_of_same_name_exists() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          # find type parameter
          ?tparam c:owner [c:attachment "def"] ; c:attachment "tparam" .
          # find references of type parameter
          [c:attachment "ref"] c:reference ?tparam ; s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def f[A](A: [[A]]) = {
            val A = 0
            A
          }
        }
      """)
  }

  @Test
  def multiple_calls_to_def() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:tpe "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def [[!Int]]f(i: [[Int]]) = 0
          [[f]](0)
          [[f]](0)
        }
      """)
  }

  @Test
  def multiple_blocks_with_same_name() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:tpe "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          def [[!Int]]f(i: [[Int]]) = 0
          [[f]]({val [[!Int]]i = 0; [[i]]})
          [[f]]({val [[!Int]]i = 0; [[i]]})
        }
      """)
  }

  @Test
  def explicit_apply_method() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:tpe "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          val [[!Option]]a = [[Option]].[[!Int]][[apply]](1)
        }
      """)
  }

  @Test
  def implicit_apply_method() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:tpe "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        class X {
          val [[!Option]]a = [[!apply]][[!Int]][[Option]](1)
        }
      """)
  }

  @Test
  def class_annotation() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:tpe "ref"] s:name ?name ; c:start ?start ; c:end ?end .
        }
      """,
      "<memory>" → """
        @[[Ann]]([[!apply]][[!Class]][[Array]]([[classOf]] [ [[X]] ]))
        class X
        class Ann(arr: [[Array]][ [[Class]] [_] ]) extends [[scala]].[[annotation]].[[StaticAnnotation]]
      """)
  }

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
}
