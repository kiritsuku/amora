package amora.backend.indexer

import org.junit.Test

import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

class ScalaDeclTest extends RestApiTest {

  @Test
  def classes() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Class/>
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
        prefix c:<http://amora.center/kb/amora/Schema/AbstractClass/>
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
        prefix c:<http://amora.center/kb/amora/Schema/Class/>
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
        prefix c:<http://amora.center/kb/amora/Schema/Object/>
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
        prefix c:<http://amora.center/kb/amora/Schema/Trait/>
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
        prefix c:<http://amora.center/kb/amora/Schema/Package/>
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
        prefix c:<http://amora.center/kb/amora/Schema/Def/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!this]]A {
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
        prefix c:<http://amora.center/kb/amora/Schema/Val/>
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
        prefix c:<http://amora.center/kb/amora/Schema/Var/>
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
        prefix c:<http://amora.center/kb/amora/Schema/LazyVal/>
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
        prefix c:<http://amora.center/kb/amora/Schema/Val/>
        select distinct ?name ?start ?end where {
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
        prefix c:<http://amora.center/kb/amora/Schema/Val/>
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

  @Test
  def multiple_lambda_decls() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Val/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f([[i]]: Int ⇒ Int) = i
          f([[v]] ⇒ v)
          f([[v]] ⇒ v)
        }
      """)
  }

  @Test
  def default_constructor() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Decl/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!this]][[X]]
      """)
  }

  @Test
  def constructor_with_parameter() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Decl/>
        select distinct * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!this]][[X]]([[value]]: Int)
      """)
  }

  @Test
  def auxiliary_constructor() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Decl/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!this]][[X]] {
          def [[this]]([[value]]: Int) {
            this()
          }
        }
      """)
  }

  @Test
  def type_alias_with_type_parameter() = {
    indexRegionData("""
        prefix c:<http://amora.center/kb/amora/Schema/Decl/>
        select * where {
          [a c:] c:name ?name ; c:posStart ?start ; c:posEnd ?end .
        }
      """,
      Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class [[!this]][[X]] {
          type [[Type]] [ [[A]] , [[B]] ] = Map[A, B]
        }
      """)
  }
}
