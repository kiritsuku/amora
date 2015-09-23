package backend.internal

import protocol.ui.Columns
import protocol.ui.Rows
import protocol.ui.Window
import protocol.ui.WindowTree

object WindowTreeCreator {

  case class WinInfo(winId: Int, x: Int, y: Int, w: Int, h: Int)
  object WinInfo {
    implicit val ord = new Ordering[WinInfo] {
      override def compare(i1: WinInfo, i2: WinInfo) = {
        if (i1.x == i2.x)
          i1.y - i2.y
        else if (i1.x < i2.x)
          i1.y - (i2.y+i2.h)
        else if (i1.y+i1.h <= i2.y)
          -1
        else
          1
      }
    }
  }

  def mkWindowTree(infos: Seq[WinInfo]): WindowTree = {
    val sorted = infos.sorted
    mkLoop(sorted)
  }

  private def mkLoop(infos: Seq[WinInfo]): WindowTree = infos match {
    case Seq(info) ⇒
      Window(s"window${info.winId}")

    case e1 +: _ +: _ ⇒
      val (classifiedElems, remainingElems) = infos.span(_.y == e1.y)
      val splitPoint = remainingElems.headOption map { h ⇒
        classifiedElems.span(_.x != h.x)
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
            mkLoop(remainingElems) match {
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
