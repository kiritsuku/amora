package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Try

import akka.actor.ActorSystem
import msgpack4z.MsgpackArray
import msgpack4z.MsgpackExt

class Nvim(val connection: Connection)(implicit val system: ActorSystem) {

  def buffers(implicit ec: ExecutionContext): Future[Seq[Int]] = {
    connection.sendRequest("vim_get_buffers", Seq()) {
      case MsgpackArray(xs) => Try {
        val types = xs.forall(_.isInstanceOf[MsgpackExt])

        if (!types)
          throw new UnexpectedResponse(s"expected: Seq[MsgpackExt], got: $xs")
        else {
          xs map { x => (x: @unchecked) match {
            case MsgpackExt(exttype, bin) =>
              bin.value.head.toInt
          }}
        }
      }

      case res =>
        Failure(new UnexpectedResponse(s"expected: MsgpackArray, got: $res"))
    }
  }
}
