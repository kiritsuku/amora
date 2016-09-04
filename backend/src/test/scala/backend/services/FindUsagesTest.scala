package backend.services

import org.junit.Test

import backend.indexer.RestApiTest
import backend.schema.Artifact
import backend.schema.Project

class FindUsagesTest extends RestApiTest {
  import backend.TestUtils._

  @Test
  def it_is_possible_to_call_FindUsages(): Unit = {
    val CursorData(cursorPos, src) = cursorData("""
      package backend.services
      class X {
        def f: Li^st[Int] = {
          val xs: List[Int] = List(1)
          xs
        }
        val xs: List[Int] = f
      }
    """)
    indexData(Artifact(Project("p"), "o", "n", "v1"), "f1.scala" → src)

    val m = serviceRequest(s"""
      @prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      @prefix registry: <http://amora.center/kb/Service/0.1/>
      @prefix request: <http://amora.center/kb/ServiceRequest/0.1/>
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
      prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      prefix decl: <http://amora.center/kb/amora/Schema/0.1/Decl/0.1/>

      select ?start ?end where {
        ?s service:result/rdf:rest*/rdf:first [decl:posStart ?start ; decl:posEnd ?end]
      }
    """) === Seq(
        Seq(Data("start", "63"), Data("end", "67")),
        Seq(Data("start", "95"), Data("end", "99")),
        Seq(Data("start", "154"), Data("end", "158")))
  }

}
