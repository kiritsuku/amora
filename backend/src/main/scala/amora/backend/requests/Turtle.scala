package amora.backend.requests

import scala.util.Failure
import scala.util.Success

import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.MediaType
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Route
import amora.backend.BackendSystem
import amora.backend.CustomContentTypes

trait Turtle extends Directives {

  def bs: BackendSystem

  def handleTurtleUpdatePostRequest(req: HttpRequest, query: String): Route = {
    req.entity.contentType.mediaType match {
      case m if m matches CustomContentTypes.`text/turtle` ⇒
        bs.runTurtleUpdate(query, "Error happened while handling Turtle update request.") {
          case Success(()) ⇒ HttpEntity(ContentTypes.`text/plain(UTF-8)`, "Update successful.")
          case Failure(t) ⇒ throw t
        }

      case m ⇒
        rejectContentType(m, CustomContentTypes.`text/turtle`)
    }
  }

  private def rejectContentType(actualContentType: MediaType, expectedContentTypes: MediaType*): Route = {
    complete(HttpResponse(StatusCodes.UnsupportedMediaType,
        entity = s"The media type was `$actualContentType` but one of the following media types is required:\n${expectedContentTypes.map(_.toString).mkString("\n")}\n"))
  }
}
