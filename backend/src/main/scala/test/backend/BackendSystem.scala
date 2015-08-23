package test.backend

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
import nvim._
import shared.test._

final class NvimAccessor(implicit system: ActorSystem) {
  val nvim = new Nvim(new Connection("127.0.0.1", 6666))

  implicit val ec = system.dispatcher
  // we cache window here in order to reduce communication overhead
  val window = nvim.currentWindow

  def handleTextChange(change: TextChange, sender: String, self: ActorRef) = {
    println(s"> received: $change")
    val newCursorPos = for {
      _ ← nvim.sendInput(change.text)
      win ← window
      c ← win.cursor
    } yield c

    newCursorPos onComplete {
      case Success(newCursorPos) ⇒
        // TODO handle multi line cursor positions
        val resp = TextChangeAnswer(change.bufferRef, newCursorPos.col, newCursorPos.col, change.text)
        self ! NvimSignal(sender, resp)
        println(s"> sent: $resp")

      case Failure(f) ⇒
        system.log.error(f, s"Failed to send response after client request `$change`.")
    }
  }

  def handleSelectionChange(change: SelectionChange, sender: String, self: ActorRef) = {
    println(s"> received: $change")
    // TODO handle multi line cursor positions
    val resp = window.flatMap(_.cursor = Position(1, change.start))

    resp onComplete {
      case Success(_) ⇒
        val resp = SelectionChangeAnswer(change.bufferRef, change.start, change.start)
        self ! NvimSignal(sender, resp)
        println(s"> sent: $resp")

      case Failure(f) ⇒
        system.log.error(f, s"Failed to send response after client request `$change`.")
    }
  }

  def handleControl(control: Control, sender: String, self: ActorRef) = {
    println(s"> received: $control")
    val newCursorPos = for {
      _ ← nvim.sendInput(control.controlSeq)
      win ← window
      c ← win.cursor
    } yield c

    newCursorPos onComplete {
      case Success(newCursorPos) ⇒
        val resp = TextChangeAnswer(control.bufferRef, newCursorPos.col, newCursorPos.col, "")
        self ! NvimSignal(sender, resp)
        println(s"> sent: $resp")

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
  private val nvim = new NvimAccessor

  override def receive = {
    case NewClient(subject) ⇒
      val sender = "client" + clients.size
      println(s"New client '$sender' seen")
      subject ! ConnectionSuccessful(sender)

    case ClientReady(sender, subject) ⇒
      if (clients.contains(sender)) {
        println(s"'$sender' already exists")
        // TODO this can only happen when multiple clients try to join at nearly the same moment
        subject ! ConnectionFailure
      }
      else {
        context.watch(subject)
        clients += sender → subject
        println(s"'$sender' joined")
        subject ! ConnectionSuccessful(sender)
      }

    case ReceivedMessage(sender, msg) ⇒
      msg match {
        case Interpret(id, expr) =>
          val res = repl.interpret(expr)
          clients(sender) ! InterpretedResult(id, res)

        case change @ SelectionChange(bufferRef, start, end) ⇒
          nvim.handleSelectionChange(change, sender, self)

        case change @ TextChange(bufferRef, start, end, text) ⇒
          nvim.handleTextChange(change, sender, self)

        case control @ Control(bufferRef, start, end, controlSeq) ⇒
          nvim.handleControl(control, sender, self)
      }

    case NvimSignal(sender, resp) ⇒
      clients(sender) ! resp

    case ClientLeft(sender) ⇒
      clients -= sender
      println(s"'$sender' left")
  }
}

sealed trait Msg
case class ReceivedMessage(sender: String, req: Request) extends Msg
case class ClientLeft(sender: String) extends Msg
case class NewClient(subject: ActorRef) extends Msg
case class ClientReady(sender: String, subject: ActorRef) extends Msg
case class NvimSignal(sender: String, resp: Response)
