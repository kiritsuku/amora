package test.backend

import java.nio.ByteBuffer

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import shared.test.Interpret
import shared.test.Person
import shared.test.Request

class BackendSystem(implicit system: ActorSystem) {

  import boopickle.Default._
  val persons = Seq(Person("myname", 50), Person("anothername", 26))
  def personBuf = Pickle.intoBytes(persons)

  val actor = system.actorOf(Props(new Actor {
    val repl = new Repl
    var clients = Map.empty[String, ActorRef]

    def receive = {
      case NewClient(sender, subject) ⇒
        context.watch(subject)
        clients += sender → subject
        println(s"'$sender' joined")
      case ReceivedMessage(sender, msg) ⇒
        println(s"'$sender' sent '$msg'")
        msg match {
          case Interpret(expr) =>
            repl.interpret(expr)
            clients(sender) ! ByteBuffer.wrap(s">>$expr<< processed".getBytes)
          case msg ⇒
            clients(sender) ! personBuf
        }
      case ClientLeft(sender) ⇒
        clients -= sender
        println(s"'$sender' left")
    }
  }))

  def sink(sender: String): Sink[Msg, Unit] = Sink.actorRef[Msg](actor, ClientLeft(sender))

  def messageFlow(sender: String): Flow[ByteBuffer, ByteBuffer, Unit] = {
    val in = Flow[ByteBuffer]
      .map(b ⇒ ReceivedMessage(sender, Unpickle[Request].fromBytes(b)))
      .to(sink(sender))
    val out = Source
      .actorRef[ByteBuffer](1, OverflowStrategy.fail)
      .mapMaterializedValue { actor ! NewClient(sender, _) }
    Flow.wrap(in, out)(Keep.none)
  }
}

sealed trait Msg
case class ReceivedMessage(sender: String, req: Request) extends Msg
case class ClientLeft(sender: String) extends Msg
case class NewClient(sender: String, subject: ActorRef) extends Msg
