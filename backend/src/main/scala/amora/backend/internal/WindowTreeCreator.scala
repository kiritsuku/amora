package amora.backend.internal

import amora.protocol.ui.Columns
import amora.protocol.ui.Rows
import amora.protocol.ui.Window
import amora.protocol.ui.WindowTree

object WindowTreeCreator {

  /**
   * `id` is the window id, `x` and `y` the position in the coordinate system,
   * `w` is the width of the window and `h` is its height.
   */
  final case class WinInfo(id: Int, x: Int, y: Int, w: Int, h: Int)
  object WinInfo {
    /**
     * This sorting algorithm is best described by an example:
     * {{{
     * ---------
     * |1|2|   |
     * ----- 5 |
     * |3|4|   |
     * ---------
     * |6| |   |
     * ---8| 9 |
     * |7| |   |
     * ---------
     * }}}
     * As one can see, elements are first sorted by their `y` axis but not
     * exclusively. The `x` axis is also considered and moves elements that
     * don't belong to the same row to the end of the row. In other words,
     * elements are sorted by rows first and then by columns.
     */
    implicit val ord = new Ordering[WinInfo] {
      override def compare(i1: WinInfo, i2: WinInfo) = {
        if (i1.x == i2.x)
          i1.y - i2.y
        else if (i1.x < i2.x)
          i1.y - (i2.y + i2.h)
        else if (i1.y + i1.h <= i2.y)
          -1
        else
          1
      }
    }
  }

  /**
   * Takes a list of window information, which is the id of the window, its
   * `x` and `y` axis and its width and height and creates a tree out of it,
   * which describes how the windows fit into a row and column model.
   */
  def mkWindowTree(infos: Seq[WinInfo]): WindowTree = {
    def group[A](infos: Seq[A])(
        headElemOp: (A, A) ⇒ Boolean,
        tupleOp: (Seq[A], Seq[A]) ⇒ Seq[Seq[A]])
        : Seq[Seq[A]] = infos match {
      case h +: t ⇒
        val f = headElemOp(h, _: A)
        val (same, remaining) = t span f
        if (remaining.isEmpty)
          Seq(h +: same)
        else
          tupleOp(h +: same, remaining) ++ group(remaining)(headElemOp, tupleOp)
    }

    /*
     * Groups all consecutive windows that form a column together to a `Seq`.
     */
    def groupCols(infos: Seq[WinInfo]) = {
      group(infos)((h, elem) ⇒ elem.y == h.y, (same, remaining) ⇒ {
        val h = remaining.head
        val (before, after) = same span (_.x != h.x)
        Seq(before, after).filter(_.nonEmpty)
      })
    }

    /*
     * Groups a `Seq` of columns to a `Seq` of rows, where each row can consist
     * of multiple `Seq` of columns.
     */
    def groupRows(infos: Seq[Seq[WinInfo]]) = {
      group(infos)((h, elem) ⇒ {
        val hh = h.head
        elem.head.y > hh.y && elem.head.x >= hh.x
      }, (same, _) ⇒ Seq(same))
    }

    /*
     * Groups a `Seq` of rows to sequences that contain the rows that form a
     * single column. The resulting `Seq` is therefore a `Seq` of rows, which
     * contain each a `Seq` of rows, which once again contain each a `Seq` of
     * columns.
     */
    def groupColRows(infos: Seq[Seq[Seq[WinInfo]]]) = {
      group(infos)((h, elem) ⇒ {
        val hh = h.head.head
        elem.head.head.y == hh.y
      }, (same, _) ⇒ Seq(same))
    }

    def loop(cols: Seq[Seq[WinInfo]]): WindowTree = {
      /* Converts `seq` to a `WindowTree`, by either using `ifEmpty` or `nonEmpty`. */
      def toTree[A](seq: Seq[A])(ifEmpty: Seq[A] ⇒ WindowTree, nonEmpty: Seq[A] ⇒ WindowTree): WindowTree =
        if (seq.lengthCompare(1) == 0)
          ifEmpty(seq)
        else
          nonEmpty(seq)

      def mkRows(rows: Seq[Seq[Seq[WinInfo]]]) = {
        val h = rows.head
        val trees = toTree(h.head.map(w ⇒ Window(w.id)))(_.head, Columns)
        toTree(h)(_ ⇒ trees, _ ⇒ loop(h.tail) match {
          case Rows(rows) ⇒ Rows(trees +: rows)
          case tree ⇒ Rows(Seq(trees, tree))
        })
      }

      def mkCols(rows: Seq[Seq[Seq[WinInfo]]]) =
        toTree(rows map loop)(_.head, Columns)

      def mkTree(rows: Seq[Seq[Seq[WinInfo]]]) =
        toTree(rows)(mkRows, mkCols)

      val rows = groupRows(cols)
      val colRows = groupColRows(rows)

      toTree(colRows map mkTree)(_.head, Rows)
    }

    val sortedInfos = infos.sorted
    val cols = groupCols(sortedInfos)
    loop(cols)
  }
}
