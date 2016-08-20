package backend.indexer

import org.junit.Test

import backend.schema.Artifact
import backend.schema.Project

class ScalaSourceIndexerTest extends RestApiTest {
  import backend.TestUtils._

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
      prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
      select ?name where {
        [a c:] c:name ?name .
      }
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
      prefix d:<http://amora.center/kb/amora/Schema/0.1/Def/0.1/>
      select ?name where {
        [a d:] d:name ?name .
      }
    """) === Seq(
        Seq(Data("name", "m1")),
        Seq(Data("name", "m2")),
        Seq(Data("name", "m3")))
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
      prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
      prefix d:<http://amora.center/kb/amora/Schema/0.1/Def/0.1/>
      select ?name where {
        ?class a c:; c:name "C1" .
        [d:owner ?class] d:name ?name .
      }
    """) === Seq(
        Seq(Data("name", "m11")),
        Seq(Data("name", "m12")))
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
      prefix amora:<http://amora.center/kb/amora/Schema/0.1/>
      prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
      select * where {
        [a c:] amora:owner [amora:name "f1.scala"]; amora:name ?name .
      }
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
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
      select ?name where {
        [a p:] p:name ?name .
      }
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
      prefix v:<http://amora.center/kb/amora/Schema/0.1/Val/0.1/>
      prefix l:<http://amora.center/kb/amora/Schema/0.1/LazyVal/0.1/>
      prefix amora:<http://amora.center/kb/amora/Schema/0.1/>
      select ?name where {
        values ?tpes {v: l:}
        [a ?tpes] amora:name ?name .
      }
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
      prefix l:<http://amora.center/kb/amora/Schema/0.1/LazyVal/0.1/>
      select ?name where {
        [a l:] l:name ?name .
      }
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
      prefix v:<http://amora.center/kb/amora/Schema/0.1/Val/0.1/>
      select ?name where {
        [a v:] v:name ?name .
      }
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
      prefix v:<http://amora.center/kb/amora/Schema/0.1/Var/0.1/>
      select ?name where {
        [a v:] v:name ?name .
      }
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
      prefix d:<http://amora.center/kb/amora/Schema/0.1/Def/0.1/>
      select ?name where {
        [a d:] d:name ?name .
      }
    """) === Seq(
        Seq(Data("name", "a")),
        Seq(Data("name", "c")))
  }
}
