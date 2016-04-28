package frontend.webui.protocol

sealed trait Request

sealed trait Response
case class ConnectionSuccessful(id: String) extends Response
case class ConnectionFailure(reason: String) extends Response
