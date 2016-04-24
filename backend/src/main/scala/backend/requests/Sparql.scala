package backend.requests

import java.net.URLDecoder

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
import akka.http.scaladsl.server.StandardRoute
import akka.http.scaladsl.server.UnacceptedResponseContentTypeRejection
import backend.BackendSystem
import backend.Content
import backend.CustomContentTypes

trait Sparql extends Directives {
  import CustomContentTypes._
  import ContentTypes._
  import MediaTypes._

  def bs: BackendSystem

  def handleSparqlGetRequest(params: Map[String, String]): StandardRoute = {
    if (params.isEmpty)
      complete(showSparqlEditor())
    else if (params.contains("query")) {
      val (ct, fmt) = params.get("format") collect {
        case "xml"  ⇒ `sparql-results+xml(UTF-8)` → ResultsFormat.FMT_RS_XML
        case "json" ⇒ `sparql-results+json(UTF-8)` → ResultsFormat.FMT_RS_JSON
        case "csv"  ⇒ `text/csv(UTF-8)` → ResultsFormat.FMT_RS_CSV
        case "tsv"  ⇒ `text/tab-separated-values(UTF-8)` → ResultsFormat.FMT_RS_TSV
      } getOrElse (`sparql-results+json(UTF-8)` → ResultsFormat.FMT_RS_JSON)
      askQuery(params("query"), ct, fmt)
    }
    else
      reject(MalformedRequestContentRejection("The parameter `query` could not be found."))
  }

  def handleSparqlPostRequest(req: HttpRequest, encodedPostReq: String): StandardRoute = {
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
          askQuery(query, ct, fmt)
        }
    }
    resp.getOrElse {
      reject(UnacceptedResponseContentTypeRejection(allMediaTypes.map(ContentNegotiator.Alternative(_))))
    }
  }

  private def showSparqlEditor() = {
    val content = Content.sparql(
      cssDeps = Seq("http://cdn.jsdelivr.net/yasgui/2.2.1/yasgui.min.css"),
      jsDeps = Seq("http://cdn.jsdelivr.net/yasgui/2.2.1/yasgui.min.js")
    )
    HttpEntity(`text/html(UTF-8)`, content)
  }

  private def askQuery(query: String, ct: ContentType.WithCharset, fmt: ResultsFormat) = {
    bs.askQuery(query, fmt) match {
      case scala.util.Success(s) ⇒
        complete(HttpEntity(ct, s))
      case scala.util.Failure(f) ⇒
        import StatusCodes._
        complete(HttpResponse(InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
    }
  }
}