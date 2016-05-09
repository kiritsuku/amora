package backend.actors

import akka.actor.Actor
import akka.actor.ActorRef
import frontend.webui.protocol._
import RequestMessage._
import backend.Content
import backend.indexer.ArtifactIndexer
import backend.Logger
import spray.json.DefaultJsonProtocol
import backend.indexer.ScalaSourceIndexer
import backend.indexer.JavaBytecodeIndexer
import akka.actor.ActorLogging
import akka.pattern.ask
import scala.concurrent.duration._
import akka.util.Timeout
import akka.actor.Props
import spray.json.RootJsonFormat
import backend.ActorLogger
import akka.stream.ActorMaterializer
import scala.concurrent.Future

class RequestActor(queue: ActorRef, indexer: ActorRef) extends Actor with ActorLogging {
  implicit val system = context.system
  implicit val timeout = Timeout(5.seconds)
  import system.dispatcher

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
      log.info(s"Client with ID `$clientId` left.")

    case ClientRequest(clientId, req) ⇒
      val sender = clients(clientId)
      handleRequest(sender, req)

    case AnonymousClientRequest(req) ⇒
      handleRequest(sender, req)
  }

  def handleRequest(sender: ActorRef, req: Request) = req match {
    case GetQueueItems ⇒
      onComplete[Seq[Int]](sender, queue.ask(QueueMessage.GetItems)) { items ⇒
        sender ! QueueItems(items)
      }
    case GetQueueItem(id) ⇒
      onComplete[Option[ActorRef]](sender, queue.ask(QueueMessage.GetItem(id))) {
        case None ⇒
          sender ! RequestFailed(s"Item with id `$id` doesn't exist.")
        case Some(ref) ⇒
          onComplete[Logger](sender, ref.ask(GetLogger)) { logger ⇒
            implicit val m = ActorMaterializer()
            var append = false
            logger.log.runForeach { str ⇒
              sender ! QueueItem(id, str, append)
              if (!append)
                append = true
            }
          }
      }
    case GetSchemas ⇒
      sender ! Schemas(Seq("artifacts", "test"), Schema("artifacts", Content.schemas.artifacts))
    case GetSchema(name) ⇒
      sender ! Schema("artifacts", Content.schemas.artifacts)
    case IndexData(json) ⇒
      handleIndexData(sender, json)
    case msg ⇒
      log.error(s"Unexpected message: $msg")
  }

  def handleIndexData(sender: ActorRef, jsonString: String) = {
    import spray.json._
    import RequestMessage.JsonProtocols._

    val json = jsonString.parseJson
    val fields = json.asJsObject.fields
    val msg = fields.getOrElse("tpe", throw new RuntimeException("Field `tpe` is missing.")) match {
      case JsString("scala-source") ⇒
        val files = json.convertTo[Files]
//        logger ⇒ handleScalaSource(files, new ScalaSourceIndexer(logger))
        ???

      case JsString("java-bytecode") ⇒
        val files = json.convertTo[Files]
//        logger ⇒ handleJavaBytecode(files, new JavaBytecodeIndexer(logger))
        ???

      case JsString("artifact") ⇒
        val artifacts = json.convertTo[Artifacts]
        val ref = system.actorOf(Props(classOf[ArtifactIndexer], indexer, new ActorLogger))
        QueueMessage.RunWithData(ref, artifacts)

      case v ⇒
        throw new RuntimeException(s"The value `$v` of field `tpe` is unknown.")
    }

    onComplete[Int](sender, queue.ask(msg)) { id ⇒
      sender ! RequestSucceeded(s"Request successfully added to worker queue. Item id: $id")
    }
  }

  def onComplete[A : reflect.ClassTag](sender: ActorRef, fut: Future[Any])(onSuccess: A ⇒ Unit): Unit = fut.mapTo[A] onComplete {
    case util.Success(v) ⇒ onSuccess(v)
    case util.Failure(f) ⇒ sender ! RequestFailed(s"Internal server error occurred while handling request: ${f.getMessage}")
  }

  private def handleScalaSource(files: Files, indexer: ScalaSourceIndexer) = {
    val res = indexer.convertToHierarchy(files.files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        this.indexer ! IndexerMessage.AddFile(fileName, hierarchy)
    }
  }

  private def handleJavaBytecode(files: Files, indexer: JavaBytecodeIndexer) = {
    val res = indexer.bytecodeToHierarchy(files.files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        this.indexer ! IndexerMessage.AddFile(fileName, hierarchy)
    }
  }

}
sealed trait RequestMessage
object RequestMessage {
  /** Used for HTTP GET and POST requests. */
  case class AnonymousClientRequest(req: Request) extends RequestMessage
  /** Used for requests over websocket. */
  case class ClientRequest(clientId: String, req: Request) extends RequestMessage
  case class ClientAuthorizationRequest(client: ActorRef) extends RequestMessage
  case class ClientLeft(clientId: String) extends RequestMessage
  case class ClientJoined(clientId: String, client: ActorRef) extends RequestMessage

  case class Files(tpe: String, files: Seq[File])
  case class File(fileName: String, src: String)
  case class Artifact(organization: String, name: String, version: String)
  case class Artifacts(tpe: String, artifacts: Seq[Artifact])
  case object GetLogger

  object JsonProtocols extends DefaultJsonProtocol {
    implicit val fileFormat: RootJsonFormat[File] = jsonFormat2(File)
    implicit val filesFormat: RootJsonFormat[Files] = jsonFormat2(Files)
    implicit val artifactFormat: RootJsonFormat[Artifact] = jsonFormat3(Artifact)
    implicit val artifactsFormat: RootJsonFormat[Artifacts] = jsonFormat2(Artifacts)
  }
}
