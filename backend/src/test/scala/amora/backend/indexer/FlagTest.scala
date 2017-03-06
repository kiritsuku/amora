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
}
