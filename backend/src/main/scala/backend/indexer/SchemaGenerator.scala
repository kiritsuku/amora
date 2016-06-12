package backend.indexer

import com.github.jsonldjava.utils.JsonUtils
import com.github.jsonldjava.core.JsonLdOptions
import com.github.jsonldjava.core.JsonLdProcessor
import spray.json._

class SchemaGenerator {

  def generate(jsonString: String): JsValue = {
    // TODO do not hardcode the name and the version of the schema
    val user = "amora"
    val amora = "http://amora.center/kb"
    val schemaName = "Artifact"
    val schemaVersion = "0.1"
    val schemaUrl = s"$amora/$user/Schema/$schemaName/"

    val rawSchema = jsonString
        .replaceAllLiterally("$AMORA", "file:///home/antoras/dev/scala/tooling-research/schema")
        .replaceAllLiterally("$ID", s"$amora/$schemaName/$schemaVersion")
    val expandedJson = JsonLdProcessor.expand(JsonUtils.fromString(rawSchema), new JsonLdOptions)
    val ctx = find(rawSchema.parseJson.asJsObject.fields, "@context") {
      case ("@context", v) ⇒ v.compactPrint
    }
    val expandedSchema = s"""{ "@context": $ctx, "@graph": ${JsonUtils.toPrettyString(expandedJson)} }""".parseJson
    val fields = expandedSchema.asJsObject.fields

    var jsCtx = Map[String, JsValue](
      schemaName → JsString(schemaUrl)
    )

    val graph = find(fields, "@graph") {
      case ("@graph", JsArray(elems)) ⇒ elems
    }
    graph.foreach {
      case JsObject(fields) ⇒
        val isMetaInformation = fields.keys.exists(_.endsWith("/schemaId"))
        if (!isMetaInformation) {
          val id = find(fields, "@id") {
            case ("@id", v) ⇒ v
          }
          val prop = find(fields, "/schemaProperty") {
            case (key, value) if key.endsWith("/schemaProperty") ⇒ value match {
              case JsString(str) ⇒ str
              case JsArray(Vector(JsObject(fields))) ⇒
                find(fields, "@value") {
                  case ("@value", JsString(str)) ⇒ str
                }
            }
          }
          val tpe = find(fields, "/schemaType") {
            case (key, value) if key.endsWith("/schemaType") ⇒ value match {
              case tpe: JsString ⇒ tpe
              case JsArray(Vector(JsObject(fields))) ⇒
                find(fields, "@id") {
                  case ("@id", v: JsString) ⇒ v
                }
            }
          }
          jsCtx += prop → JsObject(Map(
            "@id" → id,
            "@type" → tpe))
        }
      case v ⇒ throw new InvalidSchemaException(s"Can't handle JSON value: $v")
    }

    JsObject(Map("@context" → JsObject(jsCtx)))
  }

  private def find[A, B, C](map: Map[A, B], keyName: String)(pf: PartialFunction[(A, B), C]): C = {
    map.collectFirst(pf) match {
      case None ⇒ throw new InvalidSchemaException(s"Field `$keyName` not found.")
      case Some(v) ⇒ v
    }
  }
}

final class InvalidSchemaException(msg: String) extends RuntimeException(msg)
