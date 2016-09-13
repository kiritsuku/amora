package amora.backend.actors

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.control.NonFatal

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.OneForOneStrategy
import akka.actor.ReceiveTimeout
import akka.actor.SupervisorStrategy
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCode
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.RequestContext
import akka.http.scaladsl.server.RouteResult
import amora.backend.PlatformConstants

class RequestActor(
    rctx: RequestContext,
    target: ActorRef,
    msg: Any,
    errorMessage: String,
    onSuccess: Any ⇒ ToResponseMarshallable)
      extends Actor with ActorLogging {

  private val p = Promise[RouteResult]

  implicit private def d = context.system.dispatcher
  context.setReceiveTimeout(PlatformConstants.timeout.duration)
  target ! msg

  override def receive = {
    case ReceiveTimeout ⇒
      complete(StatusCodes.RequestTimeout, s"""Timeout on [$target] after ${context.receiveTimeout.toMillis}ms. Sender[$sender] sent message "$msg".""")
    case res ⇒
      try {
        val fut = onSuccess(res)(rctx.request).map(RouteResult.Complete)
        p.completeWith(fut)
        context.stop(self)
      } catch {
        case NonFatal(t) ⇒
          log.error(t, errorMessage)
          complete(StatusCodes.InternalServerError, s"Internal server error: ${t.getMessage}")
      }
  }

  override val supervisorStrategy = OneForOneStrategy() {
    case t ⇒
      log.error(t, errorMessage)
      complete(StatusCodes.InternalServerError, s"Internal server error: ${t.getMessage}")
      SupervisorStrategy.Stop
  }

  def future: Future[RouteResult] =
    p.future

  private def complete(status: StatusCode, msg: String) = {
    p.success(RouteResult.Complete(HttpResponse(status, entity = msg)))
    context.stop(self)
  }
}
