package shared.test

case class BufferRef(id: String)

sealed trait Request
case class Interpret(id: String, code: String) extends Request
case class Control(bufferRef: BufferRef, controlSeq: String) extends Request
case class TextChange(bufferRef: BufferRef, text: String) extends Request
case class SelectionChange(bufferRef: BufferRef, cursorRow: Int, cursorColumn: Int) extends Request

sealed trait Response
case class ConnectionSuccessful(name: String) extends Response
case object ConnectionFailure extends Response
case class InterpretedResult(id: String, res: String) extends Response
case class TextChangeAnswer(bufferRef: BufferRef, lines: Seq[String], cursorRow: Int, cursorColumn: Int) extends Response
case class SelectionChangeAnswer(bufferRef: BufferRef, cursorStartRow: Int, cursorStartColumn: Int, cursorEndRow: Int, cursorEndColumn: Int) extends Response
case class ClientUpdate(bufferRef: Option[BufferRef], mode: String, lines: Seq[String], cursorRow: Int, cursorColumn: Int) extends Response

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
