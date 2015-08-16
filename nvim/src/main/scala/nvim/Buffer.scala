package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import akka.actor.ActorSystem
import msgpack4z.MsgpackBinary
import msgpack4z.MsgpackUnion

final case class Buffer(id: Int, connection: Connection)(implicit system: ActorSystem) {

  def name(implicit ec: ExecutionContext): Future[String] = {
    connection.sendRequest("buffer_get_name", Seq(MsgpackUnion.int(id))) {
      case MsgpackBinary(bin) =>
        Success(new String(bin, "UTF-8"))

      case res =>
        Failure(new UnexpectedResponse(s"expected: MsgpackArray, got: $res"))
    }
  }
}
