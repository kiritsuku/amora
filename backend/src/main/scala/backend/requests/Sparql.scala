package backend.requests

import java.io.ByteArrayOutputStream
import java.net.URLDecoder

import scala.util.Failure
import scala.util.Success

import org.apache.jena.query.ResultSet
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.query.ResultSetRewindable
import org.apache.jena.sparql.resultset.ResultsFormat

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.ContentNegotiator
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.MalformedRequestContentRejection
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.RouteResult
import akka.http.scaladsl.server.UnacceptedResponseContentTypeRejection
import backend.AkkaLogging
import backend.BackendSystem
import backend.Content

trait Sparql extends Directives with AkkaLogging {
  import akka.http.scaladsl.model.ContentTypes._
  import akka.http.scaladsl.model.MediaTypes._
  import backend.CustomContentTypes._

  def bs: BackendSystem

  def retrieveJsonLdContext(path: String): Route = {
    val query = s"""
      |SELECT * WHERE {
      |  <$path> <http://amora.center/kb/amora/Schema/0.1/Format/0.1/content> ?str .
      |}
    """.stripMargin.trim
    runQuery(query) { r ⇒
      if (r.size == 0)
        HttpResponse(StatusCodes.NotFound, entity = s"No JSONLD context found for URI `$path`.")
      else {
        val str = r.next().get("str")
        require(str != null, "No field with name `str` found.")
        HttpEntity(`text/plain(UTF-8)`, str.asLiteral().getString)
      }
    }
  }

  def handleKbPathGetRequest(path: String): Route = {
    val query = s"""
      |PREFIX kb:<${Content.ModelName}>
      |PREFIX s:<http://schema.org/>
      |
      |SELECT * WHERE {
      |  <$path> ?p ?o .
      |}
      |LIMIT 100
    """.stripMargin.trim
    runQuery(query) { r ⇒
      showSparqlEditor(query, resultSetAsString(r, ResultsFormat.FMT_RS_JSON))
    }
  }

  def handleKbPathPostRequest(path: String): Route = {
    val query = s"""
      |SELECT * WHERE {
      |  <$path> ?p ?o .
      |}
      |LIMIT 100
    """.stripMargin.trim
    runQuery(query) { r ⇒
      HttpEntity(`sparql-results+json(UTF-8)`, resultSetAsString(r, ResultsFormat.FMT_RS_JSON))
    }
  }

  def handleSparqlGetRequest(params: Map[String, String]): Route = {
    val query = s"""
      |PREFIX kb:<${Content.ModelName}>
      |PREFIX s:<http://schema.org/>
      |
      |SELECT * WHERE {
      |  ?s ?p ?o .
      |}
      |LIMIT 100
    """.stripMargin.trim
    if (params.isEmpty)
      complete(showSparqlEditor(query, "{}"))
    else if (params.contains("query")) {
      val (ct, fmt) = params.get("format") collect {
        case "xml"  ⇒ `sparql-results+xml(UTF-8)` → ResultsFormat.FMT_RS_XML
        case "json" ⇒ `sparql-results+json(UTF-8)` → ResultsFormat.FMT_RS_JSON
        case "csv"  ⇒ `text/csv(UTF-8)` → ResultsFormat.FMT_RS_CSV
        case "tsv"  ⇒ `text/tab-separated-values(UTF-8)` → ResultsFormat.FMT_RS_TSV
      } getOrElse (`sparql-results+json(UTF-8)` → ResultsFormat.FMT_RS_JSON)
      runQuery(params("query")) { r ⇒
        HttpEntity(ct, resultSetAsString(r, fmt))
      }
    }
    else
      reject(MalformedRequestContentRejection("The parameter `query` could not be found."))
  }

  def handleSparqlPostRequest(req: HttpRequest, encodedPostReq: String): Route = {
    val ct = req.header[Accept].flatMap(_.mediaRanges.headOption).collect {
      case m if m matches `sparql-results+xml`  ⇒ `sparql-results+xml(UTF-8)` → ResultsFormat.FMT_RS_XML
      case m if m matches `sparql-results+json` ⇒ `sparql-results+json(UTF-8)` → ResultsFormat.FMT_RS_JSON
      case m if m matches `text/csv` ⇒ `text/csv(UTF-8)` → ResultsFormat.FMT_RS_CSV
      case m if m matches `text/tab-separated-values` ⇒ `text/tab-separated-values(UTF-8)` → ResultsFormat.FMT_RS_TSV
    }
    val resp = ct.map {
      case (ct, fmt) ⇒
        if (!encodedPostReq.startsWith("query="))
          reject(MalformedRequestContentRejection("The parameter `query` could not be found."))
        else {
          val query = URLDecoder.decode(encodedPostReq.drop("query=".length), "UTF-8")
          runQuery(query) { r ⇒
            HttpEntity(ct, resultSetAsString(r, fmt))
          }
        }
    }
    resp.getOrElse {
      reject(UnacceptedResponseContentTypeRejection(allMediaTypes.map(ContentNegotiator.Alternative(_))))
    }
  }

  def handleSparqlUpdatePostRequest(req: HttpRequest, encodedPostReq: String): Route = {
    if (!encodedPostReq.startsWith("query="))
      reject(MalformedRequestContentRejection("The parameter `query` could not be found."))
    else {
      val query = URLDecoder.decode(encodedPostReq.drop("query=".length), "UTF-8")
      onComplete(bs.runUpdate(query)) {
        case Success(()) ⇒
          complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, "Update successful."))
        case Failure(f) ⇒
          log.error(f, "Error happened while handling SPARQL update request.")
          complete(HttpResponse(StatusCodes.InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
      }
    }
  }

  private def showSparqlEditor(query: String, response: String) = {
    val content = Content.sparql(
      cssDeps = Seq("http://cdn.jsdelivr.net/yasgui/2.2.1/yasgui.min.css"),
      jsDeps = Seq("http://cdn.jsdelivr.net/yasgui/2.2.1/yasgui.min.js"),
      query,
      response
    )
    HttpEntity(`text/html(UTF-8)`, content)
  }

  private def resultSetAsString(r: ResultSet, fmt: ResultsFormat): String = {
    val s = new ByteArrayOutputStream
    ResultSetFormatter.output(s, r, fmt)
    new String(s.toByteArray(), "UTF-8")
  }

  private def runQuery(query: String)(f: ResultSetRewindable ⇒ ToResponseMarshallable): Route = {
    implicit val d = system.dispatcher
    rctx ⇒ bs.runQuery(query).flatMap(f(_)(rctx.request).map(RouteResult.Complete))
  }
}
