package backend

import java.nio.ByteBuffer

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import protocol._
import akka.NotUsed

final class BackendSystem(implicit system: ActorSystem) {
  import boopickle.Default._

  private val actor = system.actorOf(Props[MsgActor])

  def authFlow(): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    val out = Source
      .actorRef[Response](1, OverflowStrategy.fail)
      .mapMaterializedValue { actor ! NewClient(_) }
      .map(Pickle.intoBytes(_))
    Flow.fromSinkAndSource(Sink.ignore, out)
  }

  def messageFlow(sender: String): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    def sink(sender: String) = Sink.actorRef[Msg](actor, ClientLeft(sender))

    val in = Flow[ByteBuffer]
      .map(b ⇒ ReceivedMessage(sender, Unpickle[Request].fromBytes(b)))
      .to(sink(sender))
    val out = Source
      .actorRef[Response](1, OverflowStrategy.fail)
      .mapMaterializedValue { actor ! ClientReady(sender, _) }
      .map(Pickle.intoBytes(_))
    Flow.fromSinkAndSource(in, out)
  }
}

final class MsgActor extends Actor {
  import context.system

  private val repl = new Repl
  private var clients = Map.empty[String, ActorRef]
  private lazy val nvim = new NvimAccessor(self)

  override def receive = {
    case NewClient(subject) ⇒
      val sender = "client" + clients.size
      system.log.info(s"New client '$sender' seen")
      subject ! ConnectionSuccessful(sender)

    case ClientReady(sender, subject) ⇒
      if (clients.contains(sender)) {
        system.log.info(s"'$sender' already exists")
        // TODO this can only happen when multiple clients try to join at nearly the same moment
        subject ! ConnectionFailure
      }
      else {
        clients += sender → subject
        system.log.info(s"'$sender' joined")
        subject ! ConnectionSuccessful(sender)
        nvim.handleClientJoined(sender)
      }

    case ReceivedMessage(sender, msg) ⇒
      msg match {
        case Interpret(id, expr) ⇒
          val res = repl.interpret(expr)
          clients(sender) ! InterpretedResult(id, res)

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
      system.log.info(s"'$sender' left")
  }
}

sealed trait Msg
case class ReceivedMessage(sender: String, req: Request) extends Msg
case class ClientLeft(sender: String) extends Msg
case class NewClient(subject: ActorRef) extends Msg
case class ClientReady(sender: String, subject: ActorRef) extends Msg
case class NvimSignal(sender: Option[String], resp: Response)
object NvimSignal {
  def apply(sender: String, resp: Response): NvimSignal =
    NvimSignal(Some(sender), resp)

  def apply(resp: Response): NvimSignal =
    NvimSignal(None, resp)
}
