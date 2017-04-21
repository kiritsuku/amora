package amora.backend.requests

import java.io.ByteArrayOutputStream
import java.net.URLDecoder

import scala.util.Failure
import scala.util.Success

import org.apache.jena.query.ResultSet
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.query.ResultSetRewindable
import org.apache.jena.sparql.resultset.ResultsFormat

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.MediaType
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Route
import amora.api.SparqlModel
import amora.backend.AkkaLogging
import amora.backend.BackendSystem
import amora.backend.Content

trait Sparql extends Directives with AkkaLogging {
  import akka.http.scaladsl.model.ContentTypes._
  import akka.http.scaladsl.model.MediaTypes._
  import amora.backend.CustomContentTypes._

  def bs: BackendSystem

  def retrieveJsonLdContext(path: String): Route = {
    val query = s"""
      |SELECT * WHERE {
      |  <$path> <http://amora.center/kb/amora/Schema/Format/content> ?str .
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
      HttpEntity(`application/sparql-results+json(UTF-8)`, resultSetAsString(r, ResultsFormat.FMT_RS_JSON))
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
        case "xml"  ⇒ `application/sparql-results+xml(UTF-8)` → ResultsFormat.FMT_RS_XML
        case "json" ⇒ `application/sparql-results+json(UTF-8)` → ResultsFormat.FMT_RS_JSON
        case "csv"  ⇒ `text/csv(UTF-8)` → ResultsFormat.FMT_RS_CSV
        case "tsv"  ⇒ `text/tab-separated-values(UTF-8)` → ResultsFormat.FMT_RS_TSV
      } getOrElse (`application/sparql-results+json(UTF-8)` → ResultsFormat.FMT_RS_JSON)
      runQuery(params("query")) { r ⇒
        HttpEntity(ct, resultSetAsString(r, fmt))
      }
    }
    else
      rejectMissingParam("query")
  }

  def handleSparqlPostRequest(req: HttpRequest, query: String): Route = {
    val ct = req.header[Accept].flatMap(_.mediaRanges.headOption).collect {
      case m if m matches `application/sparql-results+xml`  ⇒ `application/sparql-results+xml(UTF-8)` → ResultsFormat.FMT_RS_XML
      case m if m matches `application/sparql-results+json` ⇒ `application/sparql-results+json(UTF-8)` → ResultsFormat.FMT_RS_JSON
      case m if m matches `text/csv`                        ⇒ `text/csv(UTF-8)` → ResultsFormat.FMT_RS_CSV
      case m if m matches `text/tab-separated-values`       ⇒ `text/tab-separated-values(UTF-8)` → ResultsFormat.FMT_RS_TSV
    }
    val resp = ct.map {
      case (ct, fmt) ⇒
        req.entity.contentType.mediaType match {
          case m if m matches `application/sparql-query` ⇒
            runQuery(query) { r ⇒
              HttpEntity(ct, resultSetAsString(r, fmt))
            }

          case m if m matches `application/x-www-form-urlencoded` ⇒
            if (!query.startsWith("query="))
              rejectMissingParam("query")
            else
              runQuery(URLDecoder.decode(query.drop("query=".length), "UTF-8")) { r ⇒
                HttpEntity(ct, resultSetAsString(r, fmt))
              }

          case m ⇒
            rejectContentType(m, `application/x-www-form-urlencoded`, `application/sparql-query`)
        }
    }
    resp.getOrElse {
      val mediaTypes = Set(`application/sparql-results+xml`, `application/sparql-results+json`, `text/csv`, `text/tab-separated-values`)
      complete(HttpResponse(StatusCodes.NotAcceptable,
          entity = s"Resource representation is only available with these types:\n${mediaTypes.map(_.toString).mkString("\n")}\n"))
    }
  }

  def handleSparqlUpdatePostRequest(req: HttpRequest, query: String): Route = {
    req.entity.contentType.mediaType match {
      case m if m matches `application/sparql-query` ⇒
        bs.runUpdate(query, "Error happened while handling SPARQL update request.") {
          case Success(()) ⇒ HttpEntity(`text/plain(UTF-8)`, "Update successful.")
          case Failure(t) ⇒ throw t
        }

      case m if m matches `application/x-www-form-urlencoded` ⇒
        if (!query.startsWith("query="))
          rejectMissingParam("query")
        else
          bs.runUpdate(URLDecoder.decode(query.drop("query=".length), "UTF-8"), "Error happened while handling SPARQL update request.") {
            case Success(()) ⇒ HttpEntity(`text/plain(UTF-8)`, "Update successful.")
            case Failure(t) ⇒ throw t
          }

      case m ⇒
        rejectContentType(m, `application/x-www-form-urlencoded`, `application/sparql-query`)
    }
  }

  def handleSparqlConstructPostRequest(req: HttpRequest, query: String): Route = {
    req.entity.contentType.mediaType match {
      case m if m matches `application/sparql-query` ⇒
        bs.runConstruct(query, "Error happened while handling SPARQL construct request.") {
          case Success(m: SparqlModel) ⇒
            val s = new ByteArrayOutputStream
            m.model.write(s, "TURTLE")
            HttpEntity(`text/turtle(UTF-8)`, new String(s.toByteArray(), "UTF-8"))
          case Failure(t) ⇒ throw t
        }

      case m ⇒
        rejectContentType(m, `application/sparql-query`)
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
    bs.runQuery(query, "Error happened while handling SPARQL query request.") {
      case Success(r: ResultSetRewindable) ⇒ f(r)
      case Failure(t) ⇒ throw t
    }
  }

  private def rejectMissingParam(param: String): Route =
    complete(HttpResponse(StatusCodes.BadRequest, entity = s"The parameter `$param` could not be found."))

  private def rejectContentType(actualContentType: MediaType, expectedContentTypes: MediaType*): Route = {
    complete(HttpResponse(StatusCodes.UnsupportedMediaType,
        entity = s"The media type was `$actualContentType` but one of the following media types is required:\n${expectedContentTypes.map(_.toString).mkString("\n")}\n"))
  }
}
