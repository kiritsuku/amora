package backend.services

import backend.indexer.RestApiTest
import org.junit.Test
import backend.schema.Project
import backend.schema.Artifact

class FindDeclarationTest extends RestApiTest {

  import backend.TestUtils._

  @Test
  def it_is_possible_to_call_FindDeclaration(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      class Decl
      class X {
        def decl: De^cl = ???
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    val m = serviceRequest(s"""
      @prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      @prefix registry: <http://amora.center/kb/Service/0.1/>
      @prefix request: <http://amora.center/kb/ServiceRequest/0.1/>
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
      prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      prefix decl: <http://amora.center/kb/amora/Schema/0.1/Decl/0.1/>

      select ?start ?end where {
        ?s service:result [decl:posStart ?start ; decl:posEnd ?end]
      }
    """) === Seq(
        Seq(Data("start", "13"), Data("end", "17")))
  }

}
