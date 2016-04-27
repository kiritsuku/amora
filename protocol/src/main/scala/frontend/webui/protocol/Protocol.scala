package frontend.webui.protocol

sealed trait Request

sealed trait Response
case class ConnectionSuccessful(id: String) extends Response
case object ConnectionFailure extends Response
