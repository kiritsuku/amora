package ui

sealed trait WindowTree
case class Rows(rows: Seq[WindowTree]) extends WindowTree
case class Columns(columns: Seq[WindowTree]) extends WindowTree
case class Window(divId: String) extends WindowTree

object WindowTreeCreator {

  case class WinInfo(winId: Int, row: Int, col: Int)

  def mkWindowTree(wins: Seq[WinInfo]): WindowTree = wins match {
    case Seq(WinInfo(id, _, _)) ⇒
      Rows(Seq(Window(s"window$id")))
    case WinInfo(id1, r1, c1) +: WinInfo(id2, r2, c2) +: xs ⇒
      if (r1 == r2)
        Rows(Seq(mkCols(wins)))
      else
        mkRows(wins)
  }

  private def mkRows(wins: Seq[WinInfo]): Rows = wins match {
    case Seq(WinInfo(id, _, _)) ⇒
      Rows(Seq(Window(s"window$id")))
    case WinInfo(id1, _, c1) +: WinInfo(id2, _, c2) +: xs ⇒
      val ret = Rows(Seq(Window(s"window$id1"), Window(s"window$id2")))
      val cols = mkRows(xs)
      Rows(ret.rows ++ cols.rows)
  }

  private def mkCols(wins: Seq[WinInfo]): Columns = wins match {
    case Seq(WinInfo(id, _, _)) ⇒
      Columns(Seq(Window(s"window$id")))
    case WinInfo(id1, _, c1) +: WinInfo(id2, _, c2) +: xs ⇒
      val ret = Columns(Seq(Window(s"window$id1"), Window(s"window$id2")))
      val cols = mkCols(xs)
      Columns(ret.columns ++ cols.columns)
  }

}
