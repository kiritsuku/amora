package backend.indexer

import com.github.jsonldjava.utils.JsonUtils
import com.github.jsonldjava.core.JsonLdOptions
import com.github.jsonldjava.core.JsonLdProcessor
import spray.json._

class SchemaGenerator {

  def resolveVariables(schemaName: String, jsonString: String): String = {
    // TODO replace absolute file reference by amora.center URI
    jsonString
        .replaceAllLiterally("$AMORA", "file:///home/antoras/dev/scala/tooling-research/schema")
        .replaceAllLiterally("$ID", mkAmoraSchemaId(schemaName))
  }

  def mkAmoraSchemaId(schemaName: String): String = {
    // TODO do not hardcode the version of the schema
    val user = "amora"
    val amora = "http://amora.center/kb"
    val schemaVersion = "0.1"
    s"${id(amora, user, "Schema", "0.1")}/$schemaName/$schemaVersion"
  }

  def mkInsertFormatQuery(schemaName: String, contentVar: String): String = {
    val a = "http://amora.center/kb"
    val schemaId = mkAmoraSchemaId(schemaName)
    val noPrefixSchemaId = id("", "amora", schemaName, "0.1")
    val formatId = s"${id(a, "amora", "Format", "0.1")}$noPrefixSchemaId/jsonld"
    s"""
      PREFIX a:<http://amora.center/kb/amora/Schema/>
      INSERT DATA {
        <$schemaId/> <$a/amora/Schema/0.1/format> <$formatId> .
        <$formatId> <$a/amora/Schema/0.1/Format/0.1/content> ?$contentVar .
      }
    """
  }

  def mkJsonLdContext(schemaName: String, json: String): JsObject = {
    val amora = "http://amora.center/kb"
    val schemaVersion = "0.1"
    val schemaUrl = s"$amora/amora/Schema/0.1/$schemaName/$schemaVersion"

    val expandedJson = JsonLdProcessor.expand(JsonUtils.fromString(json), new JsonLdOptions)
    val ctx = find(json.parseJson.asJsObject.fields, "@context") {
      case ("@context", v) ⇒ v.compactPrint
    }
    val expandedSchema = s"""{ "@context": $ctx, "@graph": ${JsonUtils.toString(expandedJson)} }""".parseJson
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
              case JsArray(Vector(JsObject(fields))) ⇒
                find(fields, "@value") {
                  case ("@value", JsString(str)) ⇒ str
                }
            }
          }
          val tpe = find(fields, "/schemaType") {
            case (key, value) if key.endsWith("/schemaType") ⇒  value match {
              case JsArray(Vector(JsObject(fields))) ⇒
                find(fields, "@id") {
                  case ("@id", JsString("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")) ⇒ JsString("@id")
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

  private def id(prefix: String, user: String, schemaName: String, schemaVersion: String) =
    s"$prefix/$user/$schemaName/$schemaVersion"

  private def find[A, B, C](map: Map[A, B], keyName: String)(pf: PartialFunction[(A, B), C]): C = {
    map.collectFirst(pf) match {
      case None ⇒ throw new InvalidSchemaException(s"Field `$keyName` not found.")
      case Some(v) ⇒ v
    }
  }
}

final class InvalidSchemaException(msg: String) extends RuntimeException(msg)
