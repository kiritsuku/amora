package amora.protocol

import amora.protocol.ui.WindowTree

case class BufferRef(id: Int)

sealed trait Request
case class Control(winId: Int, bufferId: Int, controlSeq: String) extends Request
case class TextChange(winId: Int, bufferId: Int, text: String) extends Request
case class SelectionChange(winId: Int, bufferId: Int, cursorRow: Int, cursorColumn: Int) extends Request

sealed trait Response
case class ConnectionSuccessful(name: String) extends Response
case object ConnectionFailure extends Response
case class TextChangeAnswer(winId: Int, bufferId: Int, lines: Seq[String], sel: Selection) extends Response
case class SelectionChangeAnswer(winId: Int, bufferId: Int, sel: Selection) extends Response
case class WindowUpdate(winId: Int, bufId: Int, lines: Seq[String], dim: WinDim) extends Response {
  override def toString =
    s"WindowUpdate(winId=$winId, bufId=$bufId, nrOfLines=${lines.size}, dim=$dim)"
}
case class ClientUpdate(windows: Seq[WindowUpdate], mode: String, sel: Selection, tree: Option[WindowTree]) extends Response {
  override def toString =
    s"ClientUpdate(windows=${windows.mkString("[", ", ", "]")}, mode=$mode, sel=$sel)"
}
case class Selection(winId: Int, bufId: Int, start: Pos, end: Pos) extends Response {
  override def toString =
    s"(winId=$winId, bufId=$bufId, start=$start, end=$end)"
}
case class Pos(row: Int, col: Int) extends Response {
  override def toString =
    s"(row=$row, col=$col)"
}
case class WinDim(x: Int, y: Int, w: Int, h: Int) extends Response {
  override def toString =
    s"(x=$x, y=$y, w=$w, h=$h)"
}

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
