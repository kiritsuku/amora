package backend.services

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import org.apache.jena.query.ResultSetFactory
import org.apache.jena.query.ResultSetRewindable

import scalaj.http.Http
import scalaj.http.HttpOptions
import backend.PlatformConstants

trait ScalaService {

  // This value is injected at runtime
  val uri: String = null

  private val timeout = HttpOptions.connTimeout(if (PlatformConstants.runsInDebugMode) 0 else 1000)

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
      .option(timeout)
    val resp = req.asString
    if (resp.code != 200)
      throw new IllegalStateException(s"SPARQL request responded with an error.\nRequest: $req\nResponse: $resp")

    val in = new ByteArrayInputStream(resp.body.getBytes(StandardCharsets.UTF_8))
    ResultSetFactory.makeRewindable(ResultSetFactory.fromJSON(in))
  }
}
