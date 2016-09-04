package backend.services

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import org.apache.jena.query.ResultSetFactory

import scalaj.http.Http
import scalaj.http.HttpOptions

class FindUsages {

//  def uri = "http://amora.center/sparql"
  def uri = "http://localhost:7777/sparql"

  def response(str: String): String = {
    str
  }

  def requestId: String = {
    "#reqId"
  }

  def run(offset: Int): String = {
    val query = s"""
      prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>

      select ?uStart ?uEnd where {
        ?ref ref:posStart ?start; ref:posEnd ?end .
        filter ($offset >= ?start && $offset <= ?end)
        ?ref ref:refToDecl ?decl .
        ?usages ref:refToDecl ?decl .
        ?usages ref:posStart ?uStart; ref:posEnd ?uEnd .
      }
    """

    val req = Http(uri)
      .postData(query)
      .header("Accept", "application/sparql-results+json")
      .header("Charset", "UTF-8")
      .option(HttpOptions.connTimeout(1000))
      .asString

    val in = new ByteArrayInputStream(req.body.getBytes(StandardCharsets.UTF_8))
    val r = ResultSetFactory.makeRewindable(ResultSetFactory.fromJSON(in))
    import scala.collection.JavaConverters._
    val positions = r.asScala.map { qs ⇒
      qs.get("uStart").asLiteral().getInt → qs.get("uEnd").asLiteral().getInt
    }

    response(s"""
      @prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      @prefix response: <http://amora.center/kb/ServiceResponse/0.1/>
      @prefix decl: <http://amora.center/kb/amora/Schema/0.1/Decl/0.1/>
      <#this>
        a response: ;
        service:requestId <$requestId> ;
        service:result ([${
          positions.map {
            case (start, end) ⇒ s"""
          decl:posStart $start ;
          decl:posEnd $end ;
        ] ["""
          }.mkString
        }]) ;
      .
    """)
  }
}
