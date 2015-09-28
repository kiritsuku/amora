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
    mkLoop(infos.sorted) match {
      case Seq(tree) ⇒ tree
      case elems ⇒ ???
    }
  }

  private def mkLoop(infos: Seq[WinInfo]): Seq[WindowTree] = infos match {
    case Seq(info) ⇒
      Seq(Window(info.winId))

    case e1 +: _ +: _ ⇒
      val (classifiedElems, remainingElems) = infos.span(_.y == e1.y)
      val splitPoint = remainingElems.headOption map { h ⇒
        // TODO what if x is always different?
        classifiedElems.span(_.x != h.x)
      }
      splitPoint match {
        case Some((colsBeforeSplit, colsAfterSplit)) ⇒
          if (colsAfterSplit.isEmpty) {
            val remainingRows = mkLoop(remainingElems) match {
              case Seq(tree) ⇒ tree
              case trees ⇒ ???// Rows(trees)
            }
            if (colsBeforeSplit.isEmpty)
              Seq(remainingRows)
            else if (colsBeforeSplit.size == 1)
              Seq(Window(colsBeforeSplit.head.winId), remainingRows)
            else
              Seq(Columns(colsBeforeSplit map (w ⇒ Window(w.winId))), remainingRows)
          }
          else {
            val remainingRows = {
              // TODO rename remainingColumns to treeOfCurrentIteration
              val remainingColumns =
                if (colsAfterSplit.size == 1)
                  Window(colsAfterSplit.head.winId)
                else
                  Columns(colsAfterSplit map (w ⇒ Window(w.winId)))
              mkLoop(remainingElems) match {
                case Seq(Rows(rows)) ⇒ Seq(Rows(remainingColumns +: rows))
                // TODO rename tree to treeOfNextIteration
                case Seq(tree) ⇒ Seq(Rows(Seq(remainingColumns, tree)))
                case Seq(rows, tree) ⇒ Seq(Rows(Seq(remainingColumns, rows)), tree)
              }
            }
            if (colsBeforeSplit.isEmpty)
              if (remainingRows.size == 1)
                remainingRows
              else
                Seq(Columns(remainingRows))
            else {
              val cols = colsBeforeSplit.map(w ⇒ Window(w.winId))
              remainingRows match {
                case Seq(rows, tree) ⇒
                  Seq(Rows(Seq(Columns(cols :+ rows), tree)))

                case Seq(tree) ⇒
                  Seq(Columns(cols ++ remainingRows))
              }
            }
          }

        case None ⇒
          Seq(Columns(classifiedElems map (w ⇒ Window(w.winId))))
      }
  }
}
