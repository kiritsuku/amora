package nvim.internal

import msgpack4z._
import msgpack4z.CodecInstances.all._

final case class Request(`type`: Int, id: Int, name: String, params: MsgpackUnion = MsgpackUnion.array(List())) {
  override def toString = {
    val sb = new StringBuilder
    sb.append("Request(\n")
    sb.append("  type: ").append(`type`).append(",\n")
    sb.append("  id: ").append(id).append(",\n")
    sb.append("  name: ").append(name).append(",\n")
    sb.append("  params: ").append(NvimHelper.msgpackUnionAsString(params, nest = 1)).append("\n")
    sb.append(")")
    sb.toString
  }
}
object Request {
  implicit val instance: MsgpackCodec[Request] = CaseCodec.codec(Request.apply _, Request.unapply _)
}

final case class Response(`type`: Int, id: Int, error: MsgpackUnion, result: MsgpackUnion) {
  override def toString = {
    val sb = new StringBuilder
    sb.append("Response(\n")
    sb.append("  type: ").append(`type`).append(",\n")
    sb.append("  id: ").append(id).append(",\n")
    sb.append("  error: ").append(NvimHelper.msgpackUnionAsString(error, nest = 1)).append(",\n")
    sb.append("  result: ").append(NvimHelper.msgpackUnionAsString(result, nest = 1)).append("\n")
    sb.append(")")
    sb.toString
  }
}
object Response {
  implicit val instance: MsgpackCodec[Response] = CaseCodec.codec(Response.apply _, Response.unapply _)
}

object NvimHelper {

  def msgpackUnionAsString(u: MsgpackUnion, nest: Int): String = {
    val sb = new StringBuilder

    def n(nest: Int) = "  "*nest

    def loop(u: MsgpackUnion, nest: Int): Unit = u match {
      case MsgpackArray(list) =>
        sb.append("[\n")
        list foreach { u =>
          sb.append(n(nest+1))
          loop(u, nest+1)
          sb.append(",\n")
        }
        sb.append(n(nest))
        sb.append("]")
      case MsgpackMap(map) =>
        sb.append("{\n")
        map foreach {
          case (k, v) =>
            sb.append(n(nest+1))
            loop(k, nest+1)
            sb.append(" -> ")
            loop(v, nest+1)
            sb.append(",\n")
        }
        sb.append(n(nest))
        sb.append("}")
      case MsgpackBinary(binary) =>
        sb.append(new String(binary, "UTF-8"))
      case MsgpackTrue =>
        sb.append("true")
      case MsgpackFalse =>
        sb.append("false")
      case MsgpackNil =>
        sb.append("nil")
      case MsgpackDouble(d) =>
        sb.append(s"double($d)")
      case MsgpackLong(l) =>
        sb.append(s"long($l)")
      case MsgpackULong(bi) =>
        sb.append(s"ulong($bi)")
      case MsgpackString(str) =>
        sb.append(s"str($str)")
      case MsgpackExt(exttype, data) =>
        sb.append(exttype).append(" : ").append(data.value.mkString("[", ", ", "]"))
    }

    loop(u, nest)
    sb.toString()
  }

}
