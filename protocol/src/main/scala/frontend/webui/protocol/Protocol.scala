package frontend.webui.protocol

sealed trait Request
case object GetQueueItems extends Request
case class GetQueueItem(id: Int) extends Request
case object GetSchemas extends Request
case class GetSchema(name: String) extends Request
case class IndexData(json: String) extends Request

sealed trait Response
case class AuthorizationGranted(id: String) extends Response
case object ConnectionSuccessful extends Response
case class ConnectionFailure(reason: String) extends Response
case class QueueItems(items: Seq[Int]) extends Response
case class QueueItem(id: Int, log: String, appendLog: Boolean) extends Response
case class Schemas(schemaNames: Seq[String], defaultSchema: Schema) extends Response
case class Schema(name: String, jsonSchema: String) extends Response
case class RequestSucceeded(msg: String) extends Response
case class RequestFailed(msg: String) extends Response
