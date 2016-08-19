package backend.indexer

import org.junit.Test

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Accept
import backend.CustomContentTypes
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
    testReq(post("http://amora.center/sparql", """
      prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
      select ?name where {
        [a c:] c:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
        Seq(Data("name", "C1")),
        Seq(Data("name", "C2")),
        Seq(Data("name", "C3")))
    }
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
    testReq(post("http://amora.center/sparql", """
      prefix d:<http://amora.center/kb/amora/Schema/0.1/Def/0.1/>
      select ?name where {
        [a d:] d:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
        Seq(Data("name", "m1")),
        Seq(Data("name", "m2")),
        Seq(Data("name", "m3")))
    }
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
    testReq(post("http://amora.center/sparql", """
      prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
      prefix d:<http://amora.center/kb/amora/Schema/0.1/Def/0.1/>
      select ?name where {
        ?class a c:; c:name "C1" .
        [d:owner ?class] d:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
        Seq(Data("name", "m11")),
        Seq(Data("name", "m12")))
    }
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
    testReq(post("http://amora.center/sparql", """
      prefix amora:<http://amora.center/kb/amora/Schema/0.1/>
      prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
      select * where {
        [a c:] amora:owner [amora:name "f1.scala"]; amora:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
        Seq(Data("name", "C1")),
        Seq(Data("name", "C2")))
    }
  }
}
