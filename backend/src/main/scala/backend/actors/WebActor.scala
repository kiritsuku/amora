package backend.actors

import akka.actor.Actor
import akka.actor.ActorRef
import frontend.webui.protocol._
import WebMessage._
import backend.Content
import backend.indexer.ArtifactIndexer
import backend.indexer.ArtifactIndexer.DownloadStatus
import backend.indexer.ArtifactIndexer.DownloadSuccess
import backend.indexer.ArtifactIndexer.DownloadError
import backend.Logger
import spray.json.DefaultJsonProtocol

class WebActor(queue: ActorRef, indexer: ActorRef) extends Actor {

  case class Artifact(organization: String, name: String, version: String)
  case class Artifacts(tpe: String, artifacts: Seq[Artifact])

  object JsonProtocols extends DefaultJsonProtocol {
    implicit val artifactFormat = jsonFormat3(Artifact)
    implicit val artifactsFormat = jsonFormat2(Artifacts)
  }
  import JsonProtocols._

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
      log.info(s"Client with ID `$clientId` left.")

    case ClientRequest(clientId, req) ⇒ req match {
      case GetQueueItems ⇒
        // Test data
        clients(clientId) ! QueueItems(Seq(1, 2))
      case GetQueueItem(id) ⇒
        clients(clientId) ! QueueItem(id, "test log output", false)
      case GetSchemas ⇒
        clients(clientId) ! Schemas(Seq("artifacts", "test"), Schema("artifacts", Content.schemas.artifacts))
      case GetSchema(name) ⇒
        clients(clientId) ! Schema("artifacts", Content.schemas.artifacts)
      case IndexData(json) ⇒
        handleIndexData(json)
      case msg ⇒
        log.error(s"Unexpected message: $msg")
    }
  }

  def handleIndexData(jsonString: String) = {
    import spray.json._
    val json = jsonString.parseJson
    val fields = json.asJsObject.fields
    val func: Logger ⇒ Unit = fields.getOrElse("tpe", throw new RuntimeException("Field `tpe` is missing.")) match {
      case JsString("artifact") ⇒
        val artifacts = json.convertTo[Artifacts]
        logger ⇒ handleArtifact(artifacts, new ArtifactIndexer(logger))

      case v ⇒
        throw new RuntimeException(s"The value `$v` of field `tpe` is unknown.")
    }

    queue ! QueueMsg.Add(func)
  }

  private def handleArtifact(artifacts: Artifacts, indexer: ArtifactIndexer) = {
    import indexer._

    val res = artifacts.artifacts.flatMap { artifact ⇒
      import artifact._
      fetchArtifact(organization, name, version)
    }
    val (errors, succs) = res.partition(_.isError)
    val succMsgs = succs.collect {
      case DownloadSuccess(artifact) ⇒
        artifact.getName
    }
    val errMsgs = errors.collect {
      case DownloadError(artifactName, reasonOpt) ⇒
        if (reasonOpt.isDefined)
          artifactName+"because of: "+reasonOpt.get
        else
          artifactName
    }
    val succMsg = if (succs.isEmpty) Nil else Seq(s"Fetched artifacts:" + succMsgs.sorted.mkString("\n  ", "\n  ", ""))
    val errMsg = if (errors.isEmpty) Nil else Seq(s"Failed to fetch artifacts:" + errMsgs.sorted.mkString("\n  ", "\n  ", ""))
    val msg = Seq(succMsg, errMsg).flatten.mkString("\n")

    logger.info(msg)

    if (errors.isEmpty)
      indexArtifacts(succs, indexer)
  }

  private def indexArtifacts(artifacts: Seq[DownloadStatus], indexer: ArtifactIndexer) = {
    import indexer._
    logger.info(s"No errors happened during fetching of artifacts. Start indexing of ${artifacts.size} artifacts now.")
    val indexed = artifacts.collect {
      case DownloadSuccess(artifact) ⇒ indexArtifact(artifact).get
    }.flatten

    logger.info(s"Indexing ${indexed.size} files.")
    indexed.zipWithIndex foreach {
      case ((name, hierarchy), i) ⇒
        logger.info(s"Indexing file $i ($name) with ${hierarchy.size} entries.")
        this.indexer ! IndexerMessage.AddFile(name, hierarchy)
    }
    logger.info(s"Successfully indexed ${artifacts.size} artifacts.")
  }

}
sealed trait WebMessage
object WebMessage {
  case class ClientRequest(clientId: String, req: Request) extends WebMessage
  case class ClientAuthorizationRequest(client: ActorRef) extends WebMessage
  case class ClientLeft(clientId: String) extends WebMessage
  case class ClientJoined(clientId: String, client: ActorRef) extends WebMessage
}
