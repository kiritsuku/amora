package backend

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.Props
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.util.Timeout
import backend.actors.NvimActor
import backend.actors.NvimMsg
import backend.actors.QueueActor
import backend.actors.QueueMsg
import akka.actor.ActorRef
import backend.actors.WebMessage
import backend.actors.WebActor
import backend.actors.IndexerActor
import frontend.webui.protocol.IndexData

final class BackendSystem(implicit system: ActorSystem)
    extends AnyRef
    with IndexerSystem {

  import boopickle.Default._
  import akka.pattern.ask
  import system.dispatcher

  private val nvim = system.actorOf(Props[NvimActor])
  private val queue = system.actorOf(Props[QueueActor])
  private val indexer = system.actorOf(Props[IndexerActor])
  private val web = system.actorOf(Props(classOf[WebActor], queue, indexer))

  implicit val timeout = Timeout(5.seconds)

  def indexData(json: String): Future[Int] = {
    web.ask(WebMessage.ClientRequest("anonymous", IndexData(json))).asInstanceOf[Future[Int]]
  }

  def addQueueItem(func: Logger ⇒ Unit): Future[Int] = {
    queue.ask(QueueMsg.Add(func)).asInstanceOf[Future[Int]]
  }

  def queueItems: Future[Seq[Int]] = {
    queue.ask(QueueMsg.GetItems).asInstanceOf[Future[Seq[Int]]]
  }

  def queueItem(id: Int): Future[Logger] = {
    queue.ask(QueueMsg.GetItem(id)).asInstanceOf[Future[Option[Logger]]].map {
      case None ⇒ throw new NoSuchElementException(s"Item with id $id doesn't exist.")
      case Some(logger) ⇒ logger
    }
  }

  def authNvimUi(): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    import NvimMsg._
    authFlow[protocol.Response](nvim ! NewClient(_))
  }

  def authWebUi(): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    import WebMessage._
    authFlow[frontend.webui.protocol.Response](web ! ClientAuthorizationRequest(_))
  }

  def nvimFlow(sender: String): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    import NvimMsg._
    import protocol._

    val in = Flow[ByteBuffer]
      .map(b ⇒ ReceivedMessage(sender, Unpickle[Request].fromBytes(b)))
      .to(Sink.actorRef[NvimMsg](nvim, ClientLeft(sender)))
    val out = Source
      .actorRef[Response](1, OverflowStrategy.fail)
      .mapMaterializedValue { nvim ! ClientReady(sender, _) }
      .map(Pickle.intoBytes(_))
    Flow.fromSinkAndSource(in, out)
  }

  def webUiFlow(clientId: String): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    import WebMessage._
    import frontend.webui.protocol._

    val in = Flow[ByteBuffer]
      .map(b ⇒ ClientRequest(clientId, Unpickle[Request].fromBytes(b)))
      .to(Sink.actorRef[WebMessage](web, ClientLeft(clientId)))
    val out = Source
      .actorRef[Response](1, OverflowStrategy.fail)
      .mapMaterializedValue { web ! ClientJoined(clientId, _) }
      .map(Pickle.intoBytes(_))
    Flow.fromSinkAndSource(in, out)
  }

  private def authFlow[A : Pickler](f: ActorRef ⇒ Unit): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    val out = Source
      .actorRef[A](1, OverflowStrategy.fail)
      .mapMaterializedValue(f)
      .map(Pickle.intoBytes(_))
    Flow.fromSinkAndSource(Sink.ignore, out)
  }
}
