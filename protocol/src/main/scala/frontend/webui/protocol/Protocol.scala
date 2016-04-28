package frontend.webui.protocol

sealed trait Request
case object GetQueueItems extends Request
case class GetQueueItem(id: Int) extends Request

sealed trait Response
case class AuthorizationGranted(id: String) extends Response
case object ConnectionSuccessful extends Response
case class ConnectionFailure(reason: String) extends Response
