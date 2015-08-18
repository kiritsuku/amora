package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import akka.actor.ActorSystem
import msgpack4z.MsgpackArray
import msgpack4z.MsgpackBinary
import msgpack4z.MsgpackLong
import msgpack4z.MsgpackUnion._

final case class Buffer(id: Int, connection: Connection)(implicit system: ActorSystem) {

  def name(implicit ec: ExecutionContext): Future[String] = {
    connection.sendRequest("buffer_get_name", int(id)) {
      case MsgpackBinary(bin) ⇒ new String(bin, "UTF-8")
    }
  }

  def lineCount(implicit ec: ExecutionContext): Future[Int] = {
    connection.sendRequest("buffer_line_count", int(id)) {
      case MsgpackLong(long) ⇒ long.toInt
    }
  }

  def lineAt(index: Int)(implicit ec: ExecutionContext): Future[String] = {
    connection.sendRequest("buffer_get_line", int(id), int(index)) {
      case MsgpackBinary(bin) ⇒ new String(bin, "UTF-8")
    }
  }

  def lineSlice
      (start: Int, end: Int, includeStart: Boolean = true, includeEnd: Boolean = false)
      (implicit ec: ExecutionContext)
      : Future[Seq[String]] = {
    connection.sendRequest("buffer_get_line_slice", int(id), int(start), int(end), bool(includeStart), bool(includeEnd)) {
      case MsgpackArray(lines) ⇒
        if (!lines.forall(_.isInstanceOf[MsgpackBinary]))
          throw new UnexpectedResponse(lines.toString)
        else
          lines map { x => (x: @unchecked) match {
            case MsgpackBinary(bin) ⇒
              new String(bin, "UTF-8")
          }}
    }
  }

}
