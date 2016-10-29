package amora.backend.services

import amora.backend.indexer.RestApiTest
import org.junit.Test
import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

class FindDeclarationTest extends RestApiTest {

  import amora.TestUtils._

  def serviceResult(cursorPos: Int) = {
   val m = serviceRequest(s"""
      @prefix service:<http://amora.center/kb/Schema/Service/> .
      @prefix registry:<http://amora.center/kb/Service/> .
      @prefix request:<http://amora.center/kb/ServiceRequest/> .
      <#this>
        a request: ;
        service:serviceId registry:FindDeclaration ;
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
      prefix service:<http://amora.center/kb/Schema/Service/>
      prefix decl:<http://amora.center/kb/amora/Schema/Decl/>

      select ?start ?end where {
        ?s service:result [decl:posStart ?start ; decl:posEnd ?end]
      }
    """)
  }

  @Test
  def find_decl_when_cursor_is_in_the_middle_of_ident(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class Decl
      class X {
        def decl: De^cl = ???
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    serviceResult(cursorPos) === Seq(
        Seq(Data("start", "13"), Data("end", "17")))
  }

  @Test
  def find_decl_when_cursor_is_at_beginning_of_ident(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class Decl
      class X {
        def decl: ^Decl = ???
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    serviceResult(cursorPos) === Seq(
        Seq(Data("start", "13"), Data("end", "17")))
  }

  @Test
  def find_decl_when_cursor_is_at_end_of_ident(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class Decl
      class X {
        def decl: Decl^ = ???
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    serviceResult(cursorPos) === Seq(
        Seq(Data("start", "13"), Data("end", "17")))
  }

  @Test
  def do_not_find_anything_when_cursor_does_not_point_to_an_ident(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class Decl
      class X {
        d^ef decl: Decl = ???
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    serviceResult(cursorPos) === Seq()
  }

  @Test
  def find_decl_when_cursor_points_at_decl(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class De^cl
      class X {
        def decl: Decl = ???
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    serviceResult(cursorPos) === Seq(
        Seq(Data("start", "13"), Data("end", "17")))
  }
}
