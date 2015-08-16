package nvim.internal

import msgpack4z._
import msgpack4z.CodecInstances.all._

case class Request(`type`: Int, id: Int, name: String, params: MsgpackUnion = MsgpackUnion.array(List()))
object Request {
  implicit val instance: MsgpackCodec[Request] = CaseCodec.codec(Request.apply _, Request.unapply _)
}

case class Response(`type`: Int, id: Int, error: MsgpackUnion, result: MsgpackUnion)
object Response {
  implicit val instance: MsgpackCodec[Response] = CaseCodec.codec(Response.apply _, Response.unapply _)
}
