package amora.backend.services

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import org.apache.jena.query.ResultSetFactory

import amora.api.SparqlResultSet
import amora.backend.PlatformConstants
import scalaj.http.Http
import scalaj.http.HttpOptions

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

  def turtleUpdate(update: String, errorMsg: ⇒ String): Unit = {
    val req = Http(s"$uri/turtle-update")
      .postData(update)
      .header("Content-Type", "text/turtle")
      .header("Charset", "UTF-8")
      .option(timeout)
    val resp = req.asString
    if (resp.code != 200)
      throw new IllegalStateException(s"$errorMsg\nTurtle update request responded with an error.\nRequest: $req\nResponse: $resp")
  }

  def sparqlUpdate(update: String, errorMsg: ⇒ String): Unit = {
    val req = Http(s"$uri/sparql-update")
      .postData(update)
      .header("Content-Type", "application/sparql-query")
      .header("Charset", "UTF-8")
      .option(timeout)
    val resp = req.asString
    if (resp.code != 200)
      throw new IllegalStateException(s"$errorMsg\nSPARQL update request responded with an error.\nRequest: $req\nResponse: $resp")
  }

  def sparqlRequest(query: String): SparqlResultSet = {
    val req = Http(s"$uri/sparql")
      .postData(query)
      .header("Content-Type", "application/sparql-query")
      .header("Accept", "application/sparql-results+json")
      .header("Charset", "UTF-8")
      .option(timeout)
    val resp = req.asString
    if (resp.code != 200)
      throw new IllegalStateException(s"SPARQL request responded with an error.\nRequest: $req\nResponse: $resp")

    val in = new ByteArrayInputStream(resp.body.getBytes(StandardCharsets.UTF_8))
    new SparqlResultSet(ResultSetFactory.makeRewindable(ResultSetFactory.fromJSON(in)))
  }
}
