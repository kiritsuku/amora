package backend.requests

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.StandardRoute
import backend.BackendSystem
import research.indexer.JavaBytecodeIndexer
import research.indexer.ScalaSourceIndexer

trait AddJson
    extends Directives
    with ScalaSourceIndexer
    with JavaBytecodeIndexer {

  def bs: BackendSystem

  import enumeratum._
  sealed trait Tpe extends EnumEntry
  object Tpe extends Enum[Tpe] {
    override val values = findValues

    case object ScalaSource extends Tpe
    case object JavaBytecode extends Tpe
  }

  def handleAddJsonRequest(jsonString: String): StandardRoute = {
    import Tpe._
    import spray.json._

    Try {
      val json = jsonString.parseJson.asJsObject
      val fields = json.fields
      val tpe = fields.getOrElse("tpe", throw new RuntimeException("Field `tpe` is missing.")) match {
        case JsString("scala-source") ⇒ ScalaSource
        case JsString("java-bytecode") ⇒ JavaBytecode
        case tpe ⇒ throw new RuntimeException(s"The value `$tpe` of field `tpe` is unknown.")
      }
      val data = fields.getOrElse("data", throw new RuntimeException("Field `data` is missing.")) match {
        case JsArray(arr) ⇒
          arr.map { v ⇒
            val fields = v.asJsObject.fields
            val fileName = fields.getOrElse("fileName", throw new RuntimeException("Field `fileName` is missing.")) match {
              case JsString(str) ⇒ str
              case v ⇒ throw new RuntimeException(s"The value `$v` of field `fileName` is unknown.")
            }
            val src = fields.getOrElse("src", throw new RuntimeException("Field `src` is missing.")) match {
              case JsString(str) ⇒ str
              case v ⇒ throw new RuntimeException(s"The value `$v` of field `src` is unknown.")
            }
            fileName → src
          }
        case data ⇒ throw new RuntimeException(s"The value `$data` of field `data` is unknown.")
      }
      val res = tpe match {
        case ScalaSource ⇒ convertToHierarchy(data)
        case JavaBytecode ⇒ bytecodeToHierarchy(data)
      }
      res match {
        case Success(res) ⇒
          res foreach {
            case (fileName, hierarchy) ⇒
              bs.addData(fileName, hierarchy) match {
                case Success(_) ⇒
                case Failure(f) ⇒
                  throw f
              }
          }
        case Failure(f) ⇒
          throw f
      }
    } match {
      case scala.util.Success(_) ⇒
        complete(s"Data successfully added.")
      case scala.util.Failure(f) ⇒
        import StatusCodes._
        complete(HttpResponse(InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
    }
  }
}
