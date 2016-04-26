package backend.requests

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Try

import akka.event.LoggingAdapter
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.StandardRoute
import backend.BackendSystem
import backend.indexer.ArtifactIndexer
import backend.indexer.JavaBytecodeIndexer
import backend.indexer.ScalaSourceIndexer
import backend.Logger
import backend.indexer.ArtifactIndexer._

trait AddJson extends Directives {

  def bs: BackendSystem
  def log: LoggingAdapter

  import spray.json._

  case class Files(tpe: String, files: Seq[File])
  case class File(fileName: String, src: String)
  case class Artifact(organization: String, name: String, version: String)
  case class Artifacts(tpe: String, artifacts: Seq[Artifact])

  object JsonProtocols extends DefaultJsonProtocol {
    implicit val fileFormat = jsonFormat2(File)
    implicit val filesFormat = jsonFormat2(Files)
    implicit val artifactFormat = jsonFormat3(Artifact)
    implicit val artifactsFormat = jsonFormat2(Artifacts)
  }
  import JsonProtocols._

  def handleAddJsonRequest(jsonString: String): StandardRoute = {
    Try {
      val f = handleJsonString(jsonString)
      Await.result(f, Duration.Inf)
    } match {
      case scala.util.Success(id) ⇒
        complete(s"Request successfully added to worker queue. Item id: $id")
      case scala.util.Failure(f) ⇒
        import StatusCodes._
        log.error(f, "Error happened while handling add-json request.")
        complete(HttpResponse(InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
    }
  }

  private def handleJsonString(jsonString: String): Future[Int] = {
    val json = jsonString.parseJson
    val fields = json.asJsObject.fields
    val func: Logger ⇒ Unit = fields.getOrElse("tpe", throw new RuntimeException("Field `tpe` is missing.")) match {
      case JsString("scala-source") ⇒
        val files = json.convertTo[Files]
        logger ⇒ handleScalaSource(files, new ScalaSourceIndexer(logger))

      case JsString("java-bytecode") ⇒
        val files = json.convertTo[Files]
        logger ⇒ handleJavaBytecode(files, new JavaBytecodeIndexer(logger))

      case JsString("artifact") ⇒
        val artifacts = json.convertTo[Artifacts]
        logger ⇒ handleArtifact(artifacts, new ArtifactIndexer(logger))

      case v ⇒
        throw new RuntimeException(s"The value `$v` of field `tpe` is unknown.")
    }

    bs.addQueueItem(func)
  }

  private def handleScalaSource(files: Files, indexer: ScalaSourceIndexer) = {
    val res = indexer.convertToHierarchy(files.files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        bs.addData(fileName, hierarchy).get
    }
  }

  private def handleJavaBytecode(files: Files, indexer: JavaBytecodeIndexer) = {
    val res = indexer.bytecodeToHierarchy(files.files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        bs.addData(fileName, hierarchy).get
    }
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
        bs.addData(name, hierarchy).get
    }
    logger.info(s"Successfully indexed ${artifacts.size} artifacts.")
  }
}
