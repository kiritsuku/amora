package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import akka.actor.ActorSystem
import msgpack4z.MsgpackBinary
import msgpack4z.MsgpackLong
import msgpack4z.MsgpackUnion

final case class Buffer(id: Int, connection: Connection)(implicit system: ActorSystem) {

  def name(implicit ec: ExecutionContext): Future[String] = {
    connection.sendRequest("buffer_get_name", msgId) {
      case MsgpackBinary(bin) => new String(bin, "UTF-8")
    }
  }

  def lineCount(implicit ec: ExecutionContext): Future[Long] = {
    connection.sendRequest("buffer_line_count", msgId) {
      case MsgpackLong(long) => long
    }
  }

  private def msgId = MsgpackUnion.int(id)
}
