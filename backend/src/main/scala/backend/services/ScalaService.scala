package backend.services

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import org.apache.jena.query.ResultSetFactory
import org.apache.jena.query.ResultSetRewindable

import scalaj.http.Http
import scalaj.http.HttpOptions

trait ScalaService {

//  def uri = "http://amora.center/sparql"
  def uri = "http://localhost:7777/sparql"

  def response(str: String): String = {
    str
  }

  def requestId: String = {
    "#reqId"
  }

  def sparqlRequest(query: String): ResultSetRewindable = {
    val req = Http(uri)
      .postData(query)
      .header("Accept", "application/sparql-results+json")
      .header("Charset", "UTF-8")
      .option(HttpOptions.connTimeout(1000))
      .asString

    val in = new ByteArrayInputStream(req.body.getBytes(StandardCharsets.UTF_8))
    ResultSetFactory.makeRewindable(ResultSetFactory.fromJSON(in))
  }
}
