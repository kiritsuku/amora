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
      case scala.util.Success(answer) ⇒
        complete(answer)
      case scala.util.Failure(f) ⇒
        import StatusCodes._
        log.error(f, "Error happened while handling add-json request.")
        complete(HttpResponse(InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
    }
  }

  private def handleJsonString(jsonString: String) = {
    val json = jsonString.parseJson
    val fields = json.asJsObject.fields
    fields.getOrElse("tpe", throw new RuntimeException("Field `tpe` is missing.")) match {
      case JsString("scala-source")  ⇒ handleScalaSource(json)
      case JsString("java-bytecode") ⇒ handleJavaBytecode(json)
      case JsString("artifact")      ⇒ handleArtifact(json)
      case v                         ⇒ throw new RuntimeException(s"The value `$v` of field `tpe` is unknown.")
    }
  }

  private def handleScalaSource(json: JsValue) = {
    val files = json.convertTo[Files]
    val res = convertToHierarchy(files.files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        bs.addData(fileName, hierarchy).get
    }
    "Data successfully added."
  }

  private def handleJavaBytecode(json: JsValue) = {
    val files = json.convertTo[Files]
    val res = bytecodeToHierarchy(files.files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        bs.addData(fileName, hierarchy).get
    }
    "Data successfully added."
  }

  private def handleArtifact(json: JsValue) = {
    val artifacts = json.convertTo[Artifacts]
    val res = artifacts.artifacts.flatMap { artifact ⇒
      import artifact._
      fetchArtifact(organization, name, version)
    }
    val messages = res.map {
      case DownloadSuccess(artifactName) ⇒
        s"Success: Found $artifactName."
      case DownloadError(artifactName, reasonOpt) ⇒
        val reason = if (reasonOpt.isDefined) s" Reason: ${reasonOpt.get}." else ""
        s"Error: Couldn't find $artifactName.$reason"
    }
    messages.sorted.mkString("\n")
  }
}
