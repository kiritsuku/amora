package backend.actors

import akka.actor.Actor
import akka.actor.ActorRef
import frontend.webui.protocol._
import WebMessage._

class WebActor extends Actor {

  private val log = context.system.log
  private var clients = Map.empty[String, ActorRef]

  override def receive = {
    case ClientAuthorizationRequest(client) ⇒
      val clientId = s"client${clients.size}"
      log.info(s"New client asks for authorization. Assigned to ID `$clientId`.")
      client ! AuthorizationGranted(clientId)

    case ClientJoined(clientId, client) ⇒
      if (clients.contains(clientId))
        client ! ConnectionFailure(s"Client with ID `$clientId` already exists. A reconnect may solve this problem.")
      else {
        clients += clientId → client
        log.info(s"Client with ID `$clientId` joined.")
        client ! ConnectionSuccessful
      }

    case ClientLeft(clientId) ⇒
      clients -= clientId
      log.info(s"Client with ID `$clientId` left")

    case ClientRequest(clientId, req) ⇒ req match {
      case GetQueueItems ⇒
        // Test data
        clients(clientId) ! QueueItems(Seq(1, 2))
      case msg ⇒
        log.error(s"Unexpected message: $msg")
    }
  }
}
sealed trait WebMessage
object WebMessage {
  case class ClientRequest(clientId: String, req: Request) extends WebMessage
  case class ClientAuthorizationRequest(client: ActorRef) extends WebMessage
  case class ClientLeft(clientId: String) extends WebMessage
  case class ClientJoined(clientId: String, client: ActorRef) extends WebMessage
}
