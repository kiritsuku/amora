package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import msgpack4z._
import msgpack4z.MsgpackUnion._
import macrame.enum
import nvim.internal.NvimHelper

object Nvim {
  val BufferId = 0
  val WindowId = 1
  val TabpageId = 2
}

final case class Nvim(connection: Connection) extends NoNvimProtocolFunctionality {

  import Nvim._

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
      case MsgpackArray(xs) ⇒ xs map {
        case MsgpackExt(BufferId, MsgpackBinary(Array(bufId))) ⇒
          Buffer(bufId.toInt, connection)
        case _ ⇒
          throw new UnexpectedResponse(xs.toString)
      }
    }
  }

  /**
   * Returns the active Nvim buffer.
   */
  def buffer(implicit ec: ExecutionContext): Future[Buffer] = {
    connection.sendRequest("vim_get_current_buffer") {
      case MsgpackExt(BufferId, MsgpackBinary(Array(bufId))) ⇒
        Buffer(bufId.toInt, connection)
    }
  }

  /**
   * Sets the active buffer to the buffer of the given `bufferId` and returns
   * the active buffer afterwards.
   */
  def buffer_=(bufferId: Int)(implicit ec: ExecutionContext): Future[Buffer] = {
    val req = connection.sendRequest("vim_set_current_buffer") {
      case _ ⇒ ()
    }
    req map (_ ⇒ Buffer(bufferId, connection))
  }

  /**
   * Returns all existing Nvim windows.
   */
  def windows(implicit ec: ExecutionContext): Future[Seq[Window]] = {
    connection.sendRequest("vim_get_windows") {
      case MsgpackArray(xs) ⇒ xs map {
        case MsgpackExt(WindowId, MsgpackBinary(Array(winId))) ⇒
          Window(winId.toInt, connection)
        case _ ⇒
          throw new UnexpectedResponse(xs.toString)
      }
    }
  }

  /**
   * Returns the active Nvim window.
   */
  def window(implicit ec: ExecutionContext): Future[Window] = {
    connection.sendRequest("vim_get_current_window") {
      case MsgpackExt(WindowId, MsgpackBinary(Array(winId))) ⇒
        Window(winId.toInt, connection)
    }
  }

  /**
   * Sets the active window to the window of the given `winId` and returns the
   * active window afterwards.
   */
  def window_=(winId: Int)(implicit ec: ExecutionContext): Future[Window] = {
    val req = connection.sendRequest("vim_set_current_window", int(winId)) {
      case _ ⇒ ()
    }
    req map (_ ⇒ Window(winId, connection))
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

trait NoNvimProtocolFunctionality {
  this: Nvim ⇒

  import Mode._

  /**
   * Returns the active Vim mode. For documentation about the possible modes run
   * `:help mode()` in Vim.
   */
  def activeMode(implicit ec: ExecutionContext): Future[Mode] = {
    eval("""mode("1")""") map { res => (res: @unchecked) match {
      case MsgpackBinary(mode) ⇒ mode.map(_.toChar).mkString match {
        case "n"             ⇒ Normal
        case "no"            ⇒ OperatorPending
        case "v"             ⇒ VisualByCharacter
        case "V"             ⇒ VisualByLine
        case s if s(0) == 22 ⇒ VisualBlockwise // 22 == CTRL-V
        case "s"             ⇒ SelectByCharacter
        case "S"             ⇒ SelectByLine
        case s if s(0) == 19 ⇒ SelectBlockwise // 19 == CTRL-S
        case "i"             ⇒ Insert
        case "R"             ⇒ Replace
        case "Rv"            ⇒ VirtualReplace
        case "c"             ⇒ CommandLine
        case "cv"            ⇒ VimExMode
        case "ce"            ⇒ NormalExMode
        case "r"             ⇒ HitEnterPrompt
        case "rm"            ⇒ MorePrompt
        case "r?"            ⇒ ConfirmQuery
        case "!"             ⇒ ExternalCommandRunning
        case s               ⇒ throw new IllegalArgumentException(s"Vim mode `$s` is unknown.")
      }
    }}
  }

  /**
   * Returns the selection of the active buffer.
   */
  def selection(implicit ec: ExecutionContext): Future[Selection] = {
    def asPos(u: MsgpackUnion) = u match {
      // TODO handle `bufnum` and `off`
      case MsgpackArray(List(MsgpackLong(bufnum), MsgpackLong(row), MsgpackLong(col), MsgpackLong(off))) ⇒
        Position(row.toInt, col.toInt)
      case s ⇒
        throw new IllegalArgumentException(s"Unknown selection result: ${NvimHelper.msgpackUnionAsString(s, 0)}")
    }

    def cursorPos = for {
      selStart ← eval("""getpos(".")""")
    } yield Selection(asPos(selStart), asPos(selStart))

    def visualLineSel = for {
      _ ← sendInput("<esc>")
      selStart ← eval("""getpos("'<")""")
      selEnd ← eval("""getpos("'>")""")
      _ ← sendInput("gv")
      end = asPos(selEnd)
      b ← buffer
      line ← b.line(end.row-1)
      len = line.length
    } yield Selection(asPos(selStart), Position(end.row, len))

    def currentSel = for {
      _ ← sendInput("<esc>")
      selStart ← eval("""getpos("'<")""")
      selEnd ← eval("""getpos("'>")""")
      _ ← sendInput("gv")
    } yield Selection(asPos(selStart), asPos(selEnd))

    activeMode flatMap {
      case VisualByCharacter | VisualBlockwise ⇒ currentSel
      case VisualByLine                        ⇒ visualLineSel
      case _                                   ⇒ cursorPos
    }
  }

}

final case class Selection(start: Position, end: Position) {
  override def toString =
    if (start == end)
      s"Selection(cursor=$start)"
    else
      s"Selection(start=$start,end=$end)"
}

/**
 * Represents all possible Vim modes. For documentation about the possible
 * modes run `:help mode()` in Vim.
 */
@enum class Mode {
  Normal
  OperatorPending
  VisualByCharacter
  VisualByLine
  VisualBlockwise
  SelectByCharacter
  SelectByLine
  SelectBlockwise
  Insert
  Replace
  VirtualReplace
  CommandLine
  VimExMode
  NormalExMode
  HitEnterPrompt
  MorePrompt
  ConfirmQuery
  ExternalCommandRunning
}
object Mode {
  def asString(mode: Mode): String =
    asStringImpl(mode)

  def asMode(s: String): Mode =
    fromStringImpl(s).getOrElse(throw new NoSuchElementException(s"Mode `$s` doesn't exist."))
}
