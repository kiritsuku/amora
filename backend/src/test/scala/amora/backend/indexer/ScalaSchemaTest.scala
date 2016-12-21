package amora.backend.indexer

import org.junit.Test

import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

class ScalaSchemaTest extends RestApiTest {
  import amora.TestUtils._

  @Test
  def find_top_level_classes() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        class C1
        class C2
        class C3
      """)
    sparqlRequest("""
      prefix c:<http://amora.center/kb/amora/Schema/Class/>
      select ?name where {
        [a c:] c:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "C1")),
        Seq(Data("name", "C2")),
        Seq(Data("name", "C3")))
  }

  @Test
  def find_methods_in_top_level_classes() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        class C1 {
          def m1 = 0
        }
        class C2 {
          def m2 = 0
        }
        class C3 {
          def m3 = 0
        }
      """)
    sparqlRequest("""
      prefix d:<http://amora.center/kb/amora/Schema/Def/>
      select ?name where {
        [a d:] d:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "m1")),
        Seq(Data("name", "m2")),
        Seq(Data("name", "m3")),
        Seq(Data("name", "this")),
        Seq(Data("name", "this")),
        Seq(Data("name", "this")))
  }

  @Test
  def find_all_methods_of_single_class() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        class C1 {
          def m11 = 0
          def m12 = 0
        }
        class C2 {
          def m2 = 0
        }
        class C3 {
          def m3 = 0
        }
      """)
    sparqlRequest("""
      prefix c:<http://amora.center/kb/amora/Schema/Class/>
      prefix d:<http://amora.center/kb/amora/Schema/Def/>
      select ?name where {
        ?class a c:; c:name "C1" .
        [d:owner ?class] d:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "m11")),
        Seq(Data("name", "m12")),
        Seq(Data("name", "this")))
  }

  @Test
  def find_classes_of_single_file() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "f1.scala" → """
        package a.b.c
        class C1
        class C2
      """,
      "f2.scala" → """
        package d.e.f
        class D1
        class D2
      """)
    sparqlRequest("""
      prefix amora:<http://amora.center/kb/amora/Schema/>
      prefix c:<http://amora.center/kb/amora/Schema/Class/>
      select * where {
        [a c:] amora:owner [amora:name "f1.scala"]; amora:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "C1")),
        Seq(Data("name", "C2")))
  }

  @Test
  def find_package_declarations() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package a.b.c
        class X
      """)
    showAmoraIndexContent()
    sparqlRequest("""
      prefix p:<http://amora.center/kb/amora/Schema/Package/>
      select ?name where {
        [a p:] p:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "a")),
        Seq(Data("name", "b")),
        Seq(Data("name", "c")))
  }

  @Test
  def find_vals_and_lazy_vals() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package pkg
        class X {
          def a = 0
          val b = 0
          def c = 0
          var d = 0
          lazy val e = 0
        }
      """)
    sparqlRequest("""
      prefix v:<http://amora.center/kb/amora/Schema/Val/>
      prefix l:<http://amora.center/kb/amora/Schema/LazyVal/>
      prefix amora:<http://amora.center/kb/amora/Schema/>
      select ?name where {
        values ?tpes {v: l:}
        [a ?tpes] amora:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "b")),
        Seq(Data("name", "e")))
  }

  @Test
  def find_lazy_vals() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package pkg
        class X {
          def a = 0
          val b = 0
          def c = 0
          var d = 0
          lazy val e = 0
        }
      """)
    sparqlRequest("""
      prefix l:<http://amora.center/kb/amora/Schema/LazyVal/>
      select ?name where {
        [a l:] l:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "e")))
  }

  @Test
  def find_vals() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package pkg
        class X {
          def a = 0
          val b = 0
          def c = 0
          var d = 0
          lazy val e = 0
        }
      """)
    sparqlRequest("""
      prefix v:<http://amora.center/kb/amora/Schema/Val/>
      select ?name where {
        [a v:] v:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "b")))
  }

  @Test
  def find_vars() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package pkg
        class X {
          def a = 0
          val b = 0
          def c = 0
          var d = 0
          lazy val e = 0
        }
      """)
    sparqlRequest("""
      prefix v:<http://amora.center/kb/amora/Schema/Var/>
      select ?name where {
        [a v:] v:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "d")))
  }

  @Test
  def find_defs() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package pkg
        class X {
          def a = 0
          val b = 0
          def c = 0
          var d = 0
          lazy val e = 0
        }
      """)
    sparqlRequest("""
      prefix d:<http://amora.center/kb/amora/Schema/Def/>
      select ?name where {
        [a d:] d:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "a")),
        Seq(Data("name", "c")),
        Seq(Data("name", "this")))
  }

  @Test
  def find_classes() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package pkg
        class A
        abstract class B
        trait C
        object D
      """)
    sparqlRequest("""
      prefix c:<http://amora.center/kb/amora/Schema/Class/>
      select ?name where {
        [a c:] c:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "A")))
  }

  @Test
  def find_abstract_classes() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package pkg
        class A
        abstract class B
        trait C
        object D
      """)
    sparqlRequest("""
      prefix c:<http://amora.center/kb/amora/Schema/AbstractClass/>
      select ?name where {
        [a c:] c:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "B")))
  }

  @Test
  def find_traits() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package pkg
        class A
        abstract class B
        trait C
        object D
      """)
    sparqlRequest("""
      prefix t:<http://amora.center/kb/amora/Schema/Trait/>
      select ?name where {
        [a t:] t:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "C")))
  }

  @Test
  def find_objects() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        package pkg
        class A
        abstract class B
        trait C
        object D
      """)
    sparqlRequest("""
      prefix o:<http://amora.center/kb/amora/Schema/Object/>
      select ?name where {
        [a o:] o:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "D")))
  }

  @Test
  def find_private_class_parameter() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X(i: Int, j: String)
      """)
    sparqlRequest("""
      prefix v:<http://amora.center/kb/amora/Schema/Val/>
      select ?name where {
        [v:flag "param"] v:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "i")),
        Seq(Data("name", "i")),
        Seq(Data("name", "j")),
        Seq(Data("name", "j")))
  }

  @Test
  def find_public_class_parameter() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X(val i: Int, val j: String) {
          val k = 0
        }
      """)
    sparqlRequest("""
      prefix v:<http://amora.center/kb/amora/Schema/Val/>
      select ?name where {
        [v:flag "param"] v:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "i")),
        Seq(Data("name", "i")),
        Seq(Data("name", "j")),
        Seq(Data("name", "j")))
  }

  @Test
  def public_class_parameter_are_vals() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X(val i: Int, val j: String) {
          val k = 0
        }
      """)
    sparqlRequest("""
      prefix v:<http://amora.center/kb/amora/Schema/Val/>
      select ?name where {
        [a v:] v:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "i")),
        Seq(Data("name", "i")),
        Seq(Data("name", "j")),
        Seq(Data("name", "j")),
        Seq(Data("name", "k")))
  }

  @Test
  def private_class_parameter_are_vals() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X(i: Int, j: String) {
          val k = 0
        }
      """)
    sparqlRequest("""
      prefix v:<http://amora.center/kb/amora/Schema/Val/>
      select ?name where {
        [a v:] v:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "i")),
        Seq(Data("name", "i")),
        Seq(Data("name", "j")),
        Seq(Data("name", "j")),
        Seq(Data("name", "k")))
  }

  @Test
  def class_parameter_can_be_vars() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X(val i: Int, var j: String) {
          val k = 0
        }
      """)
    sparqlRequest("""
      prefix v:<http://amora.center/kb/amora/Schema/Var/>
      select ?name where {
        [a v:] v:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "j")))
  }

  @Test
  def class_parameter_that_are_vars_are_marked_as_parameter() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X(val i: Int, var j: String) {
          val k = 0
        }
      """)
    sparqlRequest("""
      prefix v:<http://amora.center/kb/amora/Schema/Var/>
      select ?name where {
        [v:flag "param"] v:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "j")))
  }

  @Test
  def method_parameter_are_vals() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f(i: Int, j: String) = 0
        }
      """)
    sparqlRequest("""
      prefix v:<http://amora.center/kb/amora/Schema/Val/>
      select ?name where {
        [a v:] v:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "i")),
        Seq(Data("name", "j")))
  }

  @Test
  def method_parameter_are_parameter() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f(i: Int, j: String) = 0
        }
      """)
    sparqlRequest("""
      prefix v:<http://amora.center/kb/amora/Schema/Val/>
      select ?name where {
        [v:flag "param"] v:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "i")),
        Seq(Data("name", "j")))
  }

  @Test
  def owner_of_refs_in_if_expr() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          val b1 = true
          val b2 = true
          val b3 = true
          def f = if (b1) b2 else b3
        }
      """)
    sparqlRequest("""
      prefix a:<http://amora.center/kb/amora/Schema/>
      prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
      prefix def:<http://amora.center/kb/amora/Schema/Def/>
      select ?name where {
        [a:owner+ [a def:]] ref:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "Boolean")),
        Seq(Data("name", "b1")),
        Seq(Data("name", "b2")),
        Seq(Data("name", "b3")))
  }
}
