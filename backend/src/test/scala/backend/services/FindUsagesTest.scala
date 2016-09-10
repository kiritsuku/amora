package backend.services

import org.junit.Test

import backend.indexer.RestApiTest
import backend.schema.Artifact
import backend.schema.Project

class FindUsagesTest extends RestApiTest {
  import backend.TestUtils._

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
        ?s service:result/rdf:rest*/rdf:first [decl:posStart ?start ; decl:posEnd ?end]
      }
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
}
