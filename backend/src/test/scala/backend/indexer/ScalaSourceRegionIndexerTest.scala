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
}
