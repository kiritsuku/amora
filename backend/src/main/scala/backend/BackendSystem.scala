package backend

import java.nio.ByteBuffer

import scala.util.Failure
import scala.util.Success

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import nvim.{Selection ⇒ _, _}
import nvim.internal.Notification
import protocol.{Mode ⇒ _, _}

final class NvimAccessor(self: ActorRef)(implicit system: ActorSystem) {
  import system.dispatcher

  private val nvim = new Nvim(new Connection("127.0.0.1", 6666))

  /**
   * Whenever this is set to `true`, the next WinEnter event that is receives by
   * `handler` needs to be ignored. This is necessary because in some cases we
   * know that we changed the active window and therefore there may not be a
   * need to handle the sent WinEnter event.
   */
  @volatile
  private var ignoreNextWinEnter = false

  object events {
    val WinEnter = "_WinEnter"

    val AllEvents = Seq(WinEnter)
  }

  private val handler: Notification ⇒ Unit = n ⇒ {
    import events._
    n.method match {
      case WinEnter if ignoreNextWinEnter ⇒
        ignoreNextWinEnter = false
      case WinEnter ⇒
        val resp = clientUpdate

        resp onComplete {
          case Success(resp) ⇒
            self ! NvimSignal(resp)
            system.log.info(s"sent: $resp")

          case Failure(f) ⇒
            system.log.error(f, s"Failed to send a broadcast event.")
        }

      case _ ⇒
        system.log.warning(s"Notification for unknown event type arrived: $n")
    }
  }

  nvim.connection.addNotificationHandler(handler)
  events.AllEvents foreach nvim.subscribe
  private def currentBufferContent: Future[Seq[String]] = for {
    b ← nvim.buffer

    count ← b.lineCount
    s ← b.lineSlice(0, count)
  } yield s

  private def selection = for {
    sel ← nvim.selection
  } yield {
    val List(start, end) = List(
      Pos(sel.start.row-1, sel.start.col-1),
      Pos(sel.end.row-1, sel.end.col-1)
    ).sorted
    Selection(start, end)
  }

  private def clientUpdate = for {
    win ← nvim.window
    buf ← win.buffer
    content ← currentBufferContent
    mode ← nvim.activeMode
    s ← selection
  } yield ClientUpdate(win.id, buf.id, Mode.asString(mode), content, s)

  def handleClientJoined(sender: String): Unit = {
    val resp = clientUpdate

    resp onComplete {
      case Success(resp) ⇒
        self ! NvimSignal(sender, resp)
        system.log.info(s"sent: $resp")

      case Failure(f) ⇒
        system.log.error(f, s"Failed to send an update to the client '$sender'.")
    }
  }

  def handleTextChange(change: TextChange, sender: String): Unit = {
    system.log.info(s"received: $change")
    val resp = for {
      _ ← nvim.sendInput(change.text)
      win ← nvim.window = change.winId
      content ← currentBufferContent
      mode ← nvim.activeMode
      s ← selection
    } yield ClientUpdate(win.id, change.bufferId, Mode.asString(mode), content, s)

    resp onComplete {
      case Success(resp) ⇒
        self ! NvimSignal(sender, resp)
        system.log.info(s"sent: $resp")

      case Failure(f) ⇒
        system.log.error(f, s"Failed to send response after client request `$change`.")
    }
  }

  def handleSelectionChange(change: SelectionChange, sender: String): Unit = {
    system.log.info(s"received: $change")
    val resp = for {
      w ← nvim.window
      _ = if (w.id != change.winId) ignoreNextWinEnter = true
      win ← nvim.window = change.winId
      _ ← win.cursor = Position(change.cursorRow+1, change.cursorColumn)
      s ← selection
    } yield SelectionChangeAnswer(win.id, change.bufferId, s)

    resp onComplete {
      case Success(resp) ⇒
        self ! NvimSignal(sender, resp)
        system.log.info(s"sent: $resp")

      case Failure(f) ⇒
        system.log.error(f, s"Failed to send response after client request `$change`.")
    }
  }

  def handleControl(control: Control, sender: String): Unit = {
    system.log.info(s"received: $control")
    val resp = for {
      win ← nvim.window = control.winId
      _ ← nvim.sendInput(control.controlSeq)
      content ← currentBufferContent
      mode ← nvim.activeMode
      s ← selection
    } yield ClientUpdate(win.id, control.bufferId, Mode.asString(mode), content, s)

    resp onComplete {
      case Success(resp) ⇒
        self ! NvimSignal(sender, resp)
        system.log.info(s"sent: $resp")

      case Failure(f) ⇒
        system.log.error(f, s"Failed to send response after client request `$control`.")
    }
  }
}

final class BackendSystem(implicit system: ActorSystem) {
  import boopickle.Default._

  private val actor = system.actorOf(Props[MsgActor])

  def authFlow(): Flow[ByteBuffer, ByteBuffer, Unit] = {
    val out = Source
      .actorRef[Response](1, OverflowStrategy.fail)
      .mapMaterializedValue { actor ! NewClient(_) }
      .map(Pickle.intoBytes(_))
    Flow.wrap(Sink.ignore, out)(Keep.none)
  }

  def messageFlow(sender: String): Flow[ByteBuffer, ByteBuffer, Unit] = {
    def sink(sender: String) = Sink.actorRef[Msg](actor, ClientLeft(sender))

    val in = Flow[ByteBuffer]
      .map(b ⇒ ReceivedMessage(sender, Unpickle[Request].fromBytes(b)))
      .to(sink(sender))
    val out = Source
      .actorRef[Response](1, OverflowStrategy.fail)
      .mapMaterializedValue { actor ! ClientReady(sender, _) }
      .map(Pickle.intoBytes(_))
    Flow.wrap(in, out)(Keep.none)
  }
}

final class MsgActor extends Actor {
  import context.system

  private val repl = new Repl
  private var clients = Map.empty[String, ActorRef]
  private val nvim = new NvimAccessor(self)

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
