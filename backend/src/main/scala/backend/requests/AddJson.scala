package backend.requests

import scala.util.Try

import akka.event.LoggingAdapter
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.StandardRoute
import backend.BackendSystem
import research.indexer.ArtifactIndexer
import research.indexer.JavaBytecodeIndexer
import research.indexer.ScalaSourceIndexer

trait AddJson
    extends Directives
    with ScalaSourceIndexer
    with JavaBytecodeIndexer
    with ArtifactIndexer {

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
    Try(handleJsonString(jsonString)) match {
      case scala.util.Success(_) ⇒
        complete("Request successfully added to worker queue.")
      case scala.util.Failure(f) ⇒
        import StatusCodes._
        log.error(f, "Error happened while handling add-json request.")
        complete(HttpResponse(InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
    }
  }

  private def handleJsonString(jsonString: String): Unit = {
    val json = jsonString.parseJson
    val fields = json.asJsObject.fields
    val func = fields.getOrElse("tpe", throw new RuntimeException("Field `tpe` is missing.")) match {
      case JsString("scala-source") ⇒
        val files = json.convertTo[Files]
        () ⇒ handleScalaSource(files)

      case JsString("java-bytecode") ⇒
        val files = json.convertTo[Files]
        () ⇒ handleJavaBytecode(files)

      case JsString("artifact") ⇒
        val artifacts = json.convertTo[Artifacts]
        () ⇒ handleArtifact(artifacts)

      case v ⇒
        throw new RuntimeException(s"The value `$v` of field `tpe` is unknown.")
    }

    bs.addQueueItem(func)
  }

  private def handleScalaSource(files: Files) = {
    val res = convertToHierarchy(files.files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        bs.addData(fileName, hierarchy).get
    }
  }

  private def handleJavaBytecode(files: Files) = {
    val res = bytecodeToHierarchy(files.files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        bs.addData(fileName, hierarchy).get
    }
  }

  private def handleArtifact(artifacts: Artifacts) = {
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

    log.info(msg)

    if (errors.isEmpty)
      indexArtifacts(succs)
  }

  private def indexArtifacts(artifacts: Seq[DownloadStatus]) = {
    log.info(s"No errors happened during fetching of artifacts. Start indexing of ${artifacts.size} artifacts now.")
    val indexed = artifacts.collect {
      case DownloadSuccess(artifact) ⇒ indexArtifact(artifact).get
    }.flatten

    log.info(s"Indexing ${indexed.size} files.")
    indexed.zipWithIndex foreach {
      case ((name, hierarchy), i) ⇒
        log.info(s"Indexing file $i ($name) with ${hierarchy.size} entries.")
        bs.addData(name, hierarchy).get
    }
    log.info(s"Successfully indexed ${artifacts.size} artifacts.")
  }
}
