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

  @Test
  def expr_ordering_have_a_calledOn_property() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f = {
            List.apply(1).drop(3).take(5).sum
          }
        }
      """)
    sparqlRequest("""
      prefix Ref:<http://amora.center/kb/amora/Schema/Ref/>
      select ?name ?calledOn where {
        ?r Ref:name ?name ; Ref:posStart ?start ; Ref:posEnd ?end ; Ref:order ?order .
        filter (?start != ?end)
        optional {
          ?r Ref:calledOn ?q .
          ?q Ref:posStart ?qstart .
          ?q Ref:posEnd ?qend .
          filter (?qstart != ?qend)
          ?q Ref:name ?calledOn .
        }
      }
      order by ?order
    """) === Seq(
        Seq(Data("name", "List"), Data("calledOn", null)),
        Seq(Data("name", "apply"), Data("calledOn", "List")),
        Seq(Data("name", "drop"), Data("calledOn", "apply")),
        Seq(Data("name", "take"), Data("calledOn", "drop")),
        Seq(Data("name", "sum"), Data("calledOn", "take")))
  }

  @Test
  def implicit_this_access() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def g = 0
          def f = g
        }
      """)
    sparqlRequest("""
      prefix Ref:<http://amora.center/kb/amora/Schema/Ref/>
      select ?name where {
        [Ref:name ?name ; Ref:posStart ?start ; Ref:posEnd ?end ; Ref:order ?order] .
        filter (?start != ?end)
      }
      order by ?order
    """) === Seq(
        Seq(Data("name", "g")))
  }

  @Test
  def explicit_this_access() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def g = 0
          def f = this.g
        }
      """)
    sparqlRequest("""
      prefix Ref:<http://amora.center/kb/amora/Schema/Ref/>
      select ?name where {
        [Ref:name ?name ; Ref:posStart ?start ; Ref:posEnd ?end ; Ref:order ?order] .
        filter (?start != ?end)
      }
      order by ?order
    """) === Seq(
        Seq(Data("name", "g")))
  }

  @Test
  def multiple_exprs_in_body() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f = {
            val x1 = 0
            val x2 = 0
            var x3 = 0
            var x4 = 0
            def x5 = 0
            def x6 = 0
            lazy val x7 = 0
            lazy val x8 = 0
          }
        }
      """)
    sparqlRequest("""
      prefix Schema:<http://amora.center/kb/amora/Schema/>
      select ?name where {
        ?x Schema:codeOrder ?order .
        ?x Schema:name ?name .
      }
      order by ?order
    """) === Seq(
        Seq(Data("name", "x1")),
        Seq(Data("name", "x2")),
        Seq(Data("name", "x3")),
        Seq(Data("name", "x4")),
        Seq(Data("name", "x5")),
        Seq(Data("name", "x6")),
        Seq(Data("name", "x7")),
        Seq(Data("name", "x8")))
  }

  @Test
  def multiple_decls_and_refs_in_body() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f = {
            val x1 = 0
            println(0)
            val x2 = 0
            println(0)
          }
        }
      """)
    sparqlRequest("""
      prefix Schema:<http://amora.center/kb/amora/Schema/>
      select ?name where {
        ?x Schema:codeOrder ?order .
        ?x Schema:name ?name .
      }
      order by ?order
    """) === Seq(
        Seq(Data("name", "x1")),
        Seq(Data("name", "println")),
        Seq(Data("name", "x2")),
        Seq(Data("name", "println")))
  }

  @Test
  def assignments_have_code_order() = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class X {
          def f = {
            var x = 0
            println(0)
            x = 0
            println(0)
          }
        }
      """)
    sparqlRequest("""
      prefix Schema:<http://amora.center/kb/amora/Schema/>
      select ?name where {
        ?x Schema:codeOrder ?order .
        ?x Schema:name ?name .
      }
      order by ?order
    """) === Seq(
        Seq(Data("name", "x")),
        Seq(Data("name", "println")),
        Seq(Data("name", "x")),
        Seq(Data("name", "println")))
  }
}
