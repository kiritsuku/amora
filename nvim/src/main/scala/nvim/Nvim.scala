package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import akka.actor.ActorSystem
import msgpack4z.MsgpackArray
import msgpack4z.MsgpackExt
import msgpack4z.MsgpackUnion
import nvim.internal.NvimHelper

final case class Nvim(connection: Connection)(implicit val system: ActorSystem) {

  def apiInfo(implicit ec: ExecutionContext): Future[String] = {
    connection.sendRequest("vim_get_api_info") {
      case u ⇒ NvimHelper.msgpackUnionAsString(u, nest = 0)
    }
  }

  /**
   * Sends `cmd` to Nvim. Example usage:
   * {{{
   * nvim.sendVimCommand(":help")
   * }}}
   */
  def sendVimCommand(cmd: String): Unit =
    connection.sendNotification("vim_command", MsgpackUnion.string(cmd))

  def buffers(implicit ec: ExecutionContext): Future[Seq[Buffer]] = {
    connection.sendRequest("vim_get_buffers") {
      case MsgpackArray(xs) ⇒
        if (!xs.forall(_.isInstanceOf[MsgpackExt]))
          throw new UnexpectedResponse(xs.toString)
        else
          xs map { x => (x: @unchecked) match {
            case MsgpackExt(exttype, bin) ⇒
              val id = bin.value.head.toInt
              Buffer(id, connection)
          }}
    }
  }
}
