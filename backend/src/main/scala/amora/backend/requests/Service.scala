package amora.backend.requests

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Route
import amora.backend.AkkaLogging
import amora.backend.CustomContentTypes
import amora.backend.services.CallService

trait Service extends Directives with AkkaLogging {
  private implicit val d = system.dispatcher
  private val config = system.settings.config
  private val testMode = config.getBoolean("app.test-mode")
  private val interface = config.getString("app.interface")
  private val port = config.getInt("app.port")

  /**
   * Expects a request encoded in Turtle and returns a response encoded in Turtle.
   */
  def mkServiceRequest(ttlRequest: String): Route = {
    onComplete(Future(serviceRequest(ttlRequest))) {
      case Success(resp) ⇒
        complete(HttpEntity(CustomContentTypes.`text/turtle(UTF-8)`, resp))
      case Failure(t) ⇒
        log.error(t, "Error happened while handling service request.")
        complete(HttpResponse(StatusCodes.InternalServerError, entity = s"Internal server error: ${t.getMessage}"))
    }
  }

  private def serviceRequest(ttlRequest: String): String = {
    val s = new CallService(amoraUri, system)
    s.run(ttlRequest)
  }

  private def amoraUri = if (testMode) s"http://$interface:$port" else "http://amora.center"
}
