package backend.services

import org.junit.Test

import backend.indexer.RestApiTest
import backend.schema.Artifact
import backend.schema.Project

class FindUsagesTest extends RestApiTest {
  import backend.TestUtils._

  @Test
  def it_is_possible_to_call_FindUsages(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "f1.scala" → """
        package backend.services
        class X {
          def f: List[Int] = {
            val xs: List[Int] = List(1)
            xs
          }
          val xs: List[Int] = f
        }
      """)

    val m = serviceRequest("""
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
            service:value 72 ;
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
        Seq(Data("start", "69"), Data("end", "73")),
        Seq(Data("start", "103"), Data("end", "107")),
        Seq(Data("start", "168"), Data("end", "172")))
  }

}
