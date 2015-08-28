package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import akka.actor.ActorSystem
import msgpack4z._
import msgpack4z.MsgpackUnion._
import nvim.internal.NvimHelper

final case class Nvim(connection: Connection)(implicit val system: ActorSystem) {

  private val BufferId = 0
  private val WindowId = 1
  private val TabpageId = 2

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
    connection.sendNotification("vim_command", string(cmd))

  /**
   * Sends `input` to Nvim. If the request was successful, Nvim will answer with
   * the string length of `input`. Example usage:
   * {{{
   * nvim.sendInput("<esc>ggAhello<esc>") // return value: 18
   * }}}
   */
  def sendInput(input: String)(implicit ec: ExecutionContext): Future[Long] = {
    connection.sendRequest("vim_input", string(input)) {
      case MsgpackLong(long) => long
    }
  }

  /**
   * Returns all existing Nvim buffers.
   */
  def buffers(implicit ec: ExecutionContext): Future[Seq[Buffer]] = {
    connection.sendRequest("vim_get_buffers") {
      case MsgpackArray(xs) ⇒
        if (!xs.forall(_.isInstanceOf[MsgpackExt]))
          throw new UnexpectedResponse(xs.toString)
        else
          xs map { x => (x: @unchecked) match {
            case MsgpackExt(BufferId, bin) ⇒
              val id = bin.value.head.toInt
              Buffer(id, connection)
          }}
    }
  }

  /**
   * Returns the active Nvim buffer.
   */
  def currentBuffer(implicit ec: ExecutionContext): Future[Buffer] = {
    connection.sendRequest("vim_get_current_buffer") {
      case MsgpackExt(BufferId, bin) ⇒
        val id = bin.value.head.toInt
        Buffer(id, connection)
    }
  }

  /**
   * Returns all existing Nvim windows.
   */
  def windows(implicit ec: ExecutionContext): Future[Seq[Window]] = {
    connection.sendRequest("vim_get_windows") {
      case MsgpackArray(xs) ⇒
        if (!xs.forall(_.isInstanceOf[MsgpackExt]))
          throw new UnexpectedResponse(xs.toString)
        else
          xs map { x => (x: @unchecked) match {
            case MsgpackExt(WindowId, bin) ⇒
              val id = bin.value.head.toInt
              Window(id, connection)
          }}
    }
  }

  /**
   * Returns the current Nvim window.
   */
  def currentWindow(implicit ec: ExecutionContext): Future[Window] = {
    connection.sendRequest("vim_get_current_window") {
      case MsgpackExt(WindowId, bin) ⇒
        val id = bin.value.head.toInt
        Window(id, connection)
    }
  }

  /**
   * Evaluates a vimscript expression.
   * @example {{{
   * // get actual active vim mode
   * nvim.eval("""mode("")""")
   * }}}
   */
  def eval(expr: String)(implicit ec: ExecutionContext): Future[MsgpackUnion] = {
    connection.sendRequest("vim_eval", string(expr)) {
      case m ⇒ m
    }
  }

  /**
   * Subscribes to a Nvim event.
   * @example {{{
   * nvim.subscribe("test-event")
   * }}}
   */
  def subscribe(event: String)(implicit ec: ExecutionContext): Future[Unit] = {
    connection.sendRequest("vim_subscribe", string(event)) {
      case _ ⇒ ()
    }
  }

  /**
   * Unsubscribes from a Nvim event.
   * @example {{{
   * nvim.unsubscribe("test-event")
   * }}}
   */
  def unsubscribe(event: String)(implicit ec: ExecutionContext): Future[Unit] = {
    connection.sendRequest("vim_unsubscribe", string(event)) {
      case _ ⇒ ()
    }
  }
}
