package amora.backend

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.concurrent.Promise

import akka.NotUsed
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.RouteResult
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import amora.backend.actors.IndexerActor
import amora.backend.actors.IndexerMessage
import amora.backend.actors.NvimActor
import amora.backend.actors.NvimMsg
import amora.backend.actors.QueueActor
import amora.backend.actors.QueueMessage
import amora.backend.actors.RequestActor
import amora.backend.actors.RequestMessage
import amora.backend.actors.WebSocketRequestActor
import amora.frontend.webui.protocol.IndexData

final class BackendSystem(implicit system: ActorSystem) {

  import system.dispatcher
  import PlatformConstants.timeout
  import akka.pattern.ask
  import boopickle.Default._

  private val nvim = system.actorOf(Props(new NvimActor), "nvim")
  private val queue = system.actorOf(Props(new QueueActor), "queue")
  private val indexer = system.actorOf(Props(new IndexerActor), "indexer")
  private val requestHandler = system.actorOf(Props(new WebSocketRequestActor(queue, indexer)), "request-handler")

  def runQuery(query: String, errorMessage: String)(onSuccess: Any ⇒ ToResponseMarshallable): Route =
    mkRequestRoute(indexer, IndexerMessage.RunQuery(query), errorMessage, onSuccess)

  def runUpdate(query: String, errorMessage: String)(onSuccess: Any ⇒ ToResponseMarshallable): Route =
    mkRequestRoute(indexer, IndexerMessage.RunUpdate(query), errorMessage, onSuccess)

  def indexData(json: String): Future[amora.frontend.webui.protocol.Response] = {
    requestHandler.ask(RequestMessage.AnonymousClientRequest(IndexData(json))).mapTo[amora.frontend.webui.protocol.Response]
  }

  def queueItems: Future[Seq[Int]] = {
    queue.ask(QueueMessage.GetItems).mapTo[Seq[Int]]
  }

  def queueItem(id: Int): Future[Logger] = {
    queue.ask(QueueMessage.GetItem(id)).mapTo[Option[ActorRef]].flatMap {
      case None ⇒ throw new NoSuchElementException(s"Item with id $id doesn't exist.")
      case Some(ref) ⇒ ref.ask(RequestMessage.GetLogger).mapTo[Logger]
    }
  }

  def authNvimUi(): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    import amora.backend.actors.NvimMsg._
    authFlow[amora.protocol.Response](nvim ! NewClient(_))
  }

  def authWebUi(): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    import amora.backend.actors.RequestMessage._
    authFlow[amora.frontend.webui.protocol.Response](requestHandler ! ClientAuthorizationRequest(_))
  }

  def nvimFlow(sender: String): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    import amora.backend.actors.NvimMsg._
    import amora.protocol._

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
    import amora.backend.actors.RequestMessage._
    import amora.frontend.webui.protocol._

    val in = Flow[ByteBuffer]
      .map(b ⇒ ClientRequest(clientId, Unpickle[Request].fromBytes(b)))
      .to(Sink.actorRef[RequestMessage](requestHandler, ClientLeft(clientId)))
    val out = Source
      .actorRef[Response](1, OverflowStrategy.fail)
      .mapMaterializedValue { requestHandler ! ClientJoined(clientId, _) }
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

  private def mkRequestRoute(target: ActorRef, msg: Any, errorMessage: String, onSuccess: Any ⇒ ToResponseMarshallable): Route = { rctx ⇒
    val p = Promise[RouteResult]
    system.actorOf(Props {
      val a = new RequestActor(rctx, target, msg, errorMessage, onSuccess)
      // trying to be smart here. I had to get the future out of the actor and
      // this was the best way I could find to do so
      p.completeWith(a.future)
      a
    }, "request-actor-" + System.nanoTime)
    p.future
  }
}
