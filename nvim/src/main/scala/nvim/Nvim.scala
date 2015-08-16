package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import akka.actor.ActorSystem
import msgpack4z.MsgpackArray
import msgpack4z.MsgpackExt
import nvim.internal.NvimHelper

final case class Nvim(connection: Connection)(implicit val system: ActorSystem) {

  def apiInfo(implicit ec: ExecutionContext): Future[String] = {
    connection.sendRequest("vim_get_api_info", Seq())(u => Success(NvimHelper.msgpackUnionAsString(u, nest = 0)))
  }

  def buffers(implicit ec: ExecutionContext): Future[Seq[Buffer]] = {
    connection.sendRequest("vim_get_buffers", Seq()) {
      case MsgpackArray(xs) => Try {
        val types = xs.forall(_.isInstanceOf[MsgpackExt])

        if (!types)
          throw new UnexpectedResponse(s"expected: Seq[MsgpackExt], got: $xs")
        else {
          xs map { x => (x: @unchecked) match {
            case MsgpackExt(exttype, bin) =>
              val id = bin.value.head.toInt
              Buffer(id, connection)
          }}
        }
      }

      case res =>
        Failure(new UnexpectedResponse(s"expected: MsgpackArray, got: $res"))
    }
  }
}
