package amora.backend.requests

import java.io.ByteArrayOutputStream

import scala.util.Failure
import scala.util.Success

import org.apache.jena.query.ResultSet
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.query.ResultSetRewindable
import org.apache.jena.sparql.resultset.ResultsFormat

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.MediaType
import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Route
import amora.backend.BackendSystem
import amora.backend.CustomContentTypes

trait Nlp extends Directives {

  def bs: BackendSystem

  def handleNlqPostRequest(req: HttpRequest, query: String): Route = {
    req.entity.contentType.mediaType match {
      case m if m matches MediaTypes.`text/plain` ⇒
        bs.runNlq(query, "Error happened while handling natural language query request.") {
          case Success(r: ResultSetRewindable) ⇒
            HttpEntity(CustomContentTypes.`application/sparql-results+json(UTF-8)`, resultSetAsString(r, ResultsFormat.FMT_RS_JSON))
          case Failure(t) ⇒
            throw t
        }

      case m ⇒
        rejectContentType(m, MediaTypes.`text/plain`)
    }
  }

  private def resultSetAsString(r: ResultSet, fmt: ResultsFormat): String = {
    val s = new ByteArrayOutputStream
    ResultSetFormatter.output(s, r, fmt)
    new String(s.toByteArray(), "UTF-8")
  }

  private def rejectContentType(actualContentType: MediaType, expectedContentTypes: MediaType*): Route = {
    complete(HttpResponse(StatusCodes.UnsupportedMediaType,
        entity = s"The media type was `$actualContentType` but one of the following media types is required:\n${expectedContentTypes.map(_.toString).mkString("\n")}\n"))
  }
}
