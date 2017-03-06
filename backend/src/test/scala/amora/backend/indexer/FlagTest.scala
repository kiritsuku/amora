package amora.backend.indexer

import org.junit.Test

import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

class FlagTest extends RestApiTest {
  import amora.TestUtils._

  @Test
  def ctor() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A(i: Int) {
          def this() = this(0)
          def f = 0
        }
      """)
    sparqlRequest("""
      prefix Decl:<http://amora.center/kb/amora/Schema/Decl/>
      prefix ctor:<http://amora.center/kb/amora/Flag/constructor>
      select ?name where {
        [Decl:flag ctor:] Decl:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "this")),
        Seq(Data("name", "this")))
  }

  @Test
  def param() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A(a1: Int, a2: Int)(a3: Int, a4: Int) {
          def f(b1: Int, b2: Int)(b3: Int, b4: Int) = 0
        }
      """)
    sparqlRequest("""
      prefix Decl:<http://amora.center/kb/amora/Schema/Decl/>
      prefix param:<http://amora.center/kb/amora/Flag/param>
      select ?name where {
        [Decl:flag param:] Decl:name ?name .
      }
      order by ?name
    """) === Seq(
        Seq(Data("name", "a1")), Seq(Data("name", "a1")),
        Seq(Data("name", "a2")), Seq(Data("name", "a2")),
        Seq(Data("name", "a3")), Seq(Data("name", "a3")),
        Seq(Data("name", "a4")), Seq(Data("name", "a4")),
        Seq(Data("name", "b1")),
        Seq(Data("name", "b2")),
        Seq(Data("name", "b3")),
        Seq(Data("name", "b4")))
  }
}
