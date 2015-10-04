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
    def groupCols(infos: Seq[WinInfo]): Seq[Seq[WinInfo]] = {
      val h = infos.head
      val (same, remaining) = infos span (_.y == h.y)
      val splitPoint = remaining.headOption map { h ⇒
        same span (_.x != h.x)
      }
      splitPoint match {
        case None ⇒
          Seq(same)
        case Some((before, after)) ⇒
          if (before.isEmpty)
            after +: groupCols(remaining)
          else if (after.isEmpty)
            before +: groupCols(remaining)
          else
            before +: after +: groupCols(remaining)
      }
    }

    def groupRows(infos: Seq[Seq[WinInfo]]): Seq[Seq[Seq[WinInfo]]] = infos match {
      case h +: t ⇒
        val hh = h.head
        val (same, remaining) = t span (w ⇒ w.head.y > hh.y && w.head.x >= hh.x)
        if (remaining.isEmpty)
          Seq(h +: same)
        else
          (h +: same) +: groupRows(remaining)
    }

    def groupSuperRows(infos: Seq[Seq[Seq[WinInfo]]]): Seq[Seq[Seq[Seq[WinInfo]]]] = infos match {
      case h +: t ⇒
        val hh = h.head.head
        val (same, remaining) = t span (w ⇒ w.head.head.y == hh.y)
        if (remaining.isEmpty)
          Seq(h +: same)
        else
          (h +: same) +: groupSuperRows(remaining)
    }

    def loop(groups: Seq[Seq[WinInfo]]): WindowTree = {
      def mkRows(group: Seq[Seq[Seq[WinInfo]]]) = {
        val h = group.head
        val hh = {
          val hh = h.head.map(w ⇒ Window(w.winId))
          if (hh.size == 1)
            hh.head
          else
            Columns(hh)
        }
        if (h.size == 1)
          hh
        else {
          val ret = loop(h.tail)
          ret match {
            case Rows(rows) ⇒ Rows(hh +: rows)
            case tree ⇒ Rows(Seq(hh, tree))
          }
        }
      }

      def mkCols(group: Seq[Seq[Seq[WinInfo]]]) = {
        val ret = group map loop
        if (ret.size == 1)
          ret.head
        else
          Columns(ret)
      }

      def mkTree(group: Seq[Seq[Seq[WinInfo]]]) = {
        if (group.size == 1)
          mkRows(group)
        else
          mkCols(group)
      }

      val group = groupRows(groups)
      val superGroup = groupSuperRows(group)

      val trees = superGroup map mkTree
      if (trees.size == 1)
        trees.head
      else
        Rows(trees)
    }

    val sortedInfos = infos.sorted
    val groups = groupCols(sortedInfos)
    loop(groups)
  }
}
