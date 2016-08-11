package backend.actors

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import backend.NvimAccessor
import protocol._

final class NvimActor extends Actor with ActorLogging {
  import context.system
  import NvimMsg._

  private var clients = Map.empty[String, ActorRef]
  private lazy val nvim = new NvimAccessor(self)

  override def receive = {
    case NewClient(subject) ⇒
      val sender = "client" + clients.size
      log.info(s"New client '$sender' seen")
      subject ! ConnectionSuccessful(sender)

    case ClientReady(sender, subject) ⇒
      if (clients.contains(sender)) {
        log.info(s"'$sender' already exists")
        // TODO this can only happen when multiple clients try to join at nearly the same moment
        subject ! ConnectionFailure
      }
      else {
        clients += sender → subject
        log.info(s"'$sender' joined")
        subject ! ConnectionSuccessful(sender)
        nvim.handleClientJoined(sender)
      }

    case ReceivedMessage(sender, msg) ⇒
      msg match {
        case change: SelectionChange ⇒
          nvim.handleSelectionChange(change, sender)

        case change: TextChange ⇒
          nvim.handleTextChange(change, sender)

        case control: Control ⇒
          nvim.handleControl(control, sender)
      }

    case NvimSignal(Some(sender), resp) ⇒
      clients(sender) ! resp

    case NvimSignal(None, resp) ⇒
      clients.values foreach (_ ! resp)

    case ClientLeft(sender) ⇒
      clients -= sender
      log.info(s"'$sender' left")
  }
}

sealed trait NvimMsg
object NvimMsg {
  case class ReceivedMessage(sender: String, req: Request) extends NvimMsg
  case class ClientLeft(sender: String) extends NvimMsg
  case class NewClient(subject: ActorRef) extends NvimMsg
  case class ClientReady(sender: String, subject: ActorRef) extends NvimMsg
  case class NvimSignal(sender: Option[String], resp: Response)
  object NvimSignal {
    def apply(sender: String, resp: Response): NvimSignal =
      NvimSignal(Some(sender), resp)

    def apply(resp: Response): NvimSignal =
      NvimSignal(None, resp)
  }
}
