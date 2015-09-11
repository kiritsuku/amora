package protocol

case class BufferRef(id: Int)

sealed trait Request
case class Interpret(id: String, code: String) extends Request
case class Control(bufferId: Int, controlSeq: String) extends Request
case class TextChange(bufferId: Int, text: String) extends Request
case class SelectionChange(bufferRef: BufferRef, cursorRow: Int, cursorColumn: Int) extends Request

sealed trait Response
case class ConnectionSuccessful(name: String) extends Response
case object ConnectionFailure extends Response
case class InterpretedResult(id: String, res: String) extends Response
case class TextChangeAnswer(bufferRef: BufferRef, lines: Seq[String], sel: Selection) extends Response
case class SelectionChangeAnswer(bufferRef: BufferRef, sel: Selection) extends Response
case class ClientUpdate(bufferId: Int, mode: String, lines: Seq[String], sel: Selection) extends Response
case class Selection(start: Pos, end: Pos) extends Response
case class Pos(row: Int, col: Int) extends Response
case class FocusChange(windowId: Int, bufferId: Int) extends Response

object Pos {
  implicit object PosOrdering extends Ordering[Pos] {
    override def compare(a: Pos, b: Pos) = {
      val r = a.row - b.row
      if (r == 0) a.col - b.col else r
    }
  }
}

/**
 * Represents all possible Vim modes. For documentation about the possible
 * modes run `:help mode()` in Vim.
 */
object Mode {
  val Normal                 = "Normal"
  val OperatorPending        = "OperatorPending"
  val VisualByCharacter      = "VisualByCharacter"
  val VisualByLine           = "VisualByLine"
  val VisualBlockwise        = "VisualBlockwise"
  val SelectByCharacter      = "SelectByCharacter"
  val SelectByLine           = "SelectByLine"
  val SelectBlockwise        = "SelectBlockwise"
  val Insert                 = "Insert"
  val Replace                = "Replace"
  val VirtualReplace         = "VirtualReplace"
  val CommandLine            = "CommandLine"
  val VimExMode              = "VimExMode"
  val NormalExMode           = "NormalExMode"
  val HitEnterPrompt         = "HitEnterPrompt"
  val MorePrompt             = "MorePrompt"
  val ConfirmQuery           = "ConfirmQuery"
  val ExternalCommandRunning = "ExternalCommandRunning"
}
