package backend.internal

import protocol.ui.Columns
import protocol.ui.Rows
import protocol.ui.Window
import protocol.ui.WindowTree

object WindowTreeCreator {

  case class WinInfo(winId: Int, row: Int, col: Int)

  def mkWindowTree(wins: Seq[WinInfo]): WindowTree = wins match {
    case Seq(WinInfo(id, _, _)) ⇒
      Window(s"window$id")

    case e1 +: _ +: _ ⇒
      val (classifiedElems, remainingElems) = wins.span(_.row == e1.row)
      val splitPoint = remainingElems.headOption map { h ⇒
        classifiedElems.span(_.col != h.col)
      }
      splitPoint match {
        case Some((firstRow, secondRow)) ⇒
          if (secondRow.isEmpty)
            ???
          val remainingRows = {
            val remainingColumns =
              if (secondRow.size == 1)
                Window(s"window${secondRow.head.winId}")
              else
                Columns(secondRow map (w ⇒ Window(s"window${w.winId}")))
            mkWindowTree(remainingElems) match {
              case Rows(seq) ⇒ Rows(remainingColumns +: seq)
              case ret ⇒ Rows(Seq(remainingColumns, ret))
            }
          }
          if (firstRow.isEmpty)
            remainingRows
          else if (firstRow.size == 1)
            Columns(Seq(Window(s"window${firstRow.head.winId}"), remainingRows))
          else {
            Columns(Seq(Columns(firstRow map (w ⇒ Window(s"window${w.winId}"))), remainingRows))
          }

        case None ⇒
          Columns(classifiedElems map (w ⇒ Window(s"window${w.winId}")))
      }
  }
}
