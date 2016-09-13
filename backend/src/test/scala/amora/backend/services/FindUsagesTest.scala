package amora.backend.services

import org.junit.Test

import amora.backend.indexer.RestApiTest
import amora.backend.schema.Artifact
import amora.backend.schema.Project

class FindUsagesTest extends RestApiTest {
  import amora.backend.TestUtils._

  def serviceResult(cursorPos: Int) = {
    val m = serviceRequest(s"""
      @prefix service:<http://amora.center/kb/Schema/Service/0.1/> .
      @prefix registry:<http://amora.center/kb/Service/0.1/> .
      @prefix request:<http://amora.center/kb/ServiceRequest/0.1/> .
      <#this>
        a request: ;
        service:serviceId registry:FindUsages ;
        service:method [
          service:name "run" ;
          service:param [
            service:name "offset" ;
            service:value $cursorPos ;
          ] ;
        ] ;
      .
    """)

    modelAsData(m, """
      prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      prefix service:<http://amora.center/kb/Schema/Service/0.1/>
      prefix decl:<http://amora.center/kb/amora/Schema/0.1/Decl/0.1/>

      select ?start ?end where {
        ?s service:result [decl:posStart ?start ; decl:posEnd ?end]
      }
      order by ?start ?end
    """)
  }

  @Test
  def find_usages_when_cursor_is_in_the_middle_of_ident(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class X {
        val xs: Li^st[Int] = List(1)
        val ys: List[Int] = xs
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    serviceResult(cursorPos) === Seq(
        Seq(Data("start", "33"), Data("end", "37")),
        Seq(Data("start", "69"), Data("end", "73")))
  }

  @Test
  def find_usages_when_cursor_is_at_the_beginning_of_ident(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class X {
        val xs: ^List[Int] = List(1)
        val ys: List[Int] = xs
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    serviceResult(cursorPos) === Seq(
        Seq(Data("start", "33"), Data("end", "37")),
        Seq(Data("start", "69"), Data("end", "73")))
  }

  @Test
  def find_usages_when_cursor_is_at_the_end_of_ident(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class X {
        val xs: List^[Int] = List(1)
        val ys: List[Int] = xs
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    serviceResult(cursorPos) === Seq(
        Seq(Data("start", "33"), Data("end", "37")),
        Seq(Data("start", "69"), Data("end", "73")))
  }

  @Test
  def do_not_find_inferred_usages(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class X {
        val x = 0
        val y: I^nt = 0
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    serviceResult(cursorPos) === Seq(
        Seq(Data("start", "50"), Data("end", "53")))
  }

  @Test
  def do_not_use_inferred_references_as_search_object(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class X {
        // we have an inferred type `Int` here but we want to search for `List`
        val xs: List[Int] = ^List(1)
        val ys: List[Int] = xs
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    serviceResult(cursorPos) === Seq(
        Seq(Data("start", "125"), Data("end", "129")))
  }

  @Test
  def find_usages_when_cursor_is_at_declaration(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class X {
        val x^s: List[Int] = List(1)
        val ys: List[Int] = xs
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    serviceResult(cursorPos) === Seq(
        Seq(Data("start", "29"), Data("end", "31")),
        Seq(Data("start", "81"), Data("end", "83")))
  }

  @Test
  def find_usages_only_of_current_file(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class Y {
        val x = new X
        val xs = x.x^s
        val ys = x.xs
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"),
        "f1.scala" → """
          class X {
            val xs: List[Int] = List(1)
            val ys: List[Int] = xs
          }
        """,
        "f2.scala" → src)

    serviceResult(cursorPos) === Seq(
        Seq(Data("start", "58"), Data("end", "60")),
        Seq(Data("start", "80"), Data("end", "82")))
  }
}
