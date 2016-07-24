package backend.requests

import java.io.ByteArrayOutputStream

import java.net.URLDecoder

import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.query.ResultSetRewindable
import org.apache.jena.sparql.resultset.ResultsFormat

import akka.http.scaladsl.model.ContentType
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.ContentNegotiator
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.MalformedRequestContentRejection
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.UnacceptedResponseContentTypeRejection
import backend.BackendSystem
import backend.Content
import backend.CustomContentTypes

trait Sparql extends Directives {
  import CustomContentTypes._
  import ContentTypes._
  import MediaTypes._

  def bs: BackendSystem

  def retrieveJsonLdContext(path: String): Route = {
    val query = s"""
      |SELECT * WHERE {
      |  <$path> <http://amora.center/kb/amora/Schema/0.1/Format/0.1/content> ?str .
      |}
    """.stripMargin.trim
    runQuery(query) { r ⇒
      if (r.size == 0)
        complete(HttpResponse(StatusCodes.NotFound, entity = s"No JSONLD context found for URI `$path`."))
      else {
        val str = r.next().get("str")
        require(str != null, "No field with name `str` found.")
        complete(HttpEntity(`text/plain(UTF-8)`, str.asLiteral().getString))
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
    runQuery(query, ResultsFormat.FMT_RS_JSON) { sparqlResp ⇒
      complete(showSparqlEditor(query, sparqlResp))
    }
  }

  def handleKbPathPostRequest(path: String): Route = {
    val query = s"""
      |SELECT * WHERE {
      |  <$path> ?p ?o .
      |}
      |LIMIT 100
    """.stripMargin.trim
    runQuery(query, `sparql-results+json(UTF-8)`, ResultsFormat.FMT_RS_JSON)
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
      runQuery(params("query"), ct, fmt)
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
          runQuery(query, ct, fmt)
        }
    }
    resp.getOrElse {
      reject(UnacceptedResponseContentTypeRejection(allMediaTypes.map(ContentNegotiator.Alternative(_))))
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

  private def runQuery(query: String, ct: ContentType.WithCharset, fmt: ResultsFormat): Route = {
    runQuery(query, fmt) { sparqlResp ⇒
      complete(HttpEntity(ct, sparqlResp))
    }
  }

  private def runQuery(query: String, fmt: ResultsFormat)(f: String ⇒ Route): Route = {
    onComplete(bs.runQuery(query)) {
      case scala.util.Success(r) ⇒
        val s = new ByteArrayOutputStream
        ResultSetFormatter.output(s, r, fmt)
        val str = new String(s.toByteArray(), "UTF-8")
        f(str)
      case scala.util.Failure(f) ⇒
        import StatusCodes._
        complete(HttpResponse(InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
    }
  }

  private def runQuery(query: String)(f: ResultSetRewindable ⇒ Route): Route = {
    onComplete(bs.runQuery(query)) {
      case scala.util.Success(r) ⇒
        f(r)
      case scala.util.Failure(f) ⇒
        import StatusCodes._
        complete(HttpResponse(InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
    }
  }
}
