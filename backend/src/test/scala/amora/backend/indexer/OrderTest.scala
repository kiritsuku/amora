package amora.backend.indexer

import org.junit.Test

import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

class OrderTest extends RestApiTest {
  import amora.TestUtils._

  @Test
  def expr_ordering() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f = {
            List.apply(1).drop(3).take(5).sum
          }
        }
      """)
    sparqlRequest("""
      prefix Decl:<http://amora.center/kb/amora/Schema/Decl/>
      prefix Ref:<http://amora.center/kb/amora/Schema/Ref/>
      select ?name where {
        [Ref:name ?name ; Ref:posStart ?start ; Ref:posEnd ?end ; Ref:order ?order] .
        filter (?start != ?end)
      }
      order by ?order
    """) === Seq(
        Seq(Data("name", "List")),
        Seq(Data("name", "apply")),
        Seq(Data("name", "drop")),
        Seq(Data("name", "take")),
        Seq(Data("name", "sum")))
  }

  @Test
  def expr_ordering_with_implicit_apply_method() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f = {
            List(1).drop(3).take(5).sum
          }
        }
      """)
    sparqlRequest("""
      prefix Decl:<http://amora.center/kb/amora/Schema/Decl/>
      prefix Ref:<http://amora.center/kb/amora/Schema/Ref/>
      select ?name where {
        [Ref:name ?name ; Ref:posStart ?start ; Ref:posEnd ?end ; Ref:order ?order] .
        filter (?start != ?end)
      }
      order by ?order
    """) === Seq(
        Seq(Data("name", "List")),
        Seq(Data("name", "drop")),
        Seq(Data("name", "take")),
        Seq(Data("name", "sum")))
  }
}
