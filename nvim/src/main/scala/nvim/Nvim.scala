package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import akka.actor.ActorSystem
import msgpack4z.MsgpackArray
import msgpack4z.MsgpackExt
import msgpack4z.MsgpackLong
import msgpack4z.MsgpackUnion
import nvim.internal.NvimHelper

final case class Nvim(connection: Connection)(implicit val system: ActorSystem) {

  def apiInfo(implicit ec: ExecutionContext): Future[String] = {
    connection.sendRequest("vim_get_api_info") {
      case u ⇒ NvimHelper.msgpackUnionAsString(u, nest = 0)
    }
  }

  /**
   * Sends `cmd` to Nvim. If the command does not exist, this function doesn't
   * report back an error message. Example usage:
   * {{{
   * nvim.sendVimCommand(":help")
   * // This is more or less equivalent to:
   * nvim.sendInput("<esc>:help<cr>")
   * }}}
   */
  def sendVimCommand(cmd: String): Unit =
    connection.sendNotification("vim_command", MsgpackUnion.string(cmd))

  /**
   * Sends `input` to Nvim. If the request was successful, Nvim will answer with
   * the string length of `input`. Example usage:
   * {{{
   * nvim.sendInput("<esc>ggAhello<esc>") // return value: 18
   * }}}
   */
  def sendInput(input: String)(implicit ec: ExecutionContext): Future[Long] = {
    connection.sendRequest("vim_input", MsgpackUnion.string(input)) {
      case MsgpackLong(long) => long
    }
  }

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
