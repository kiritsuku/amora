package backend.indexer

import org.junit.Test

import backend.schema.Artifact
import backend.schema.Project

class ScalaSourceRegionIndexerTest extends RestApiTest {

  @Test
  def classes() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        class [[A]]
        class [[B_?]]
        class [[??]]
        class [[`hello world`]]
        object O
        abstract class AC
      """)
  }

  @Test
  def abstract_classes() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/AbstractClass/0.1/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        class A
        object O
        abstract class [[AC]] {}
      """)
  }

  @Test
  def classes_with_body() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
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
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/Object/0.1/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        object [[`x y`]] {
          def g = 0
        }
      """)
  }

  @Test
  def traits() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/Trait/0.1/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        trait [[B]] {}
      """)
  }

  @Test
  def packages() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package [[a]].[[b]].[[c]]
        class A
      """)
  }

  @Test
  def defs() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/Def/0.1/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
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
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/Val/0.1/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
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
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/Var/0.1/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
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
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/LazyVal/0.1/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
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
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/Val/0.1/>
        select * where {
          [c:flag "param"] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A([[value1]]: Int, [[`second val`]]: String)
      """)
  }

  @Test
  def method_parameters() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/0.1/Val/0.1/>
        select * where {
          [c:flag "param"] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f([[param1]]: Int)([[`p a r a m`]]: String)([[p]]: Int) = 0
        }
      """)
  }

  // ====================================================================
  // Ref tests
  // ====================================================================

  @Test
  def return_type_at_members() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
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
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
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
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
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
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
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
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          val [[!Class]]a = [[classOf]][ /* Int */ [[Int]] ]
        }
      """)
  }

  @Test
  def refs_of_imports() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
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
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        import [[scala]].[[collection]].[[mutable]].{ [[Buffer]] ⇒ [[B]], [[ListBuffer]] }
        class X {
          [[B]]
          [[ListBuffer]]
        }
      """)
  }

  @Test
  def self_ref_with_fully_qualified_name() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        trait X {
          self: [[scala]].[[collection]].[[mutable]].[[AbstractSet]][ [[java]].[[io]].[[File]] ] ⇒
        }
      """)
  }

  @Test
  def refs_in_if_expr() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
        select * where {
          [a ref:] ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
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
    indexRegionData("""
        prefix def:<http://amora.center/kb/amora/Schema/0.1/Def/0.1/>
        prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
        select * where {
          ?def def:jvmSignature "(IF)I" .
          [a ref:] ref:owner ?def ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f(i: Int) = i
          def [[!Int]]f(i: Int, s: Float) = [[i]]
        }
      """)
  }

  @Test
  def refs_of_parameter() = {
    indexRegionData("""
        prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
        prefix decl:<http://amora.center/kb/amora/Schema/0.1/Decl/0.1/>
        select * where {
          [a ref:] ref:refToDecl [decl:flag "param"] ; ref:name ?name ; ref:posStart ?start ; ref:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f(i: Int) = {
            [[i]]
          }
        }
      """)
  }
}
