package backend
package internal

import org.junit.Test

class WindowTreeTest {
  import WindowTreeCreator._
  import TestUtils._

  private def infos(pos: (Int, Int)*): Seq[WinInfo] = {
    pos.zipWithIndex map {
      case ((row, col), i) ⇒ WinInfo(i+1, row, col)
    }
  }

  @Test
  def single_window() = {
    /*
     ---
     | |
     ---
    */
    val tree = mkWindowTree(infos((0, 0)))
    tree === Window("window1")
  }

  @Test
  def multiple_windows_in_a_single_row() = {
    /*
     -----
     | | |
     -----
    */
    val tree = mkWindowTree(infos((0, 0), (0, 1), (0, 2)))
    tree === Columns(Seq(Window("window1"), Window("window2"), Window("window3")))
  }

  @Test
  def multiple_windows_in_a_single_column() = {
    /*
     ---
     | |
     ---
     | |
     ---
    */
    val tree = mkWindowTree(infos((0, 0), (1, 0), (2, 0)))
    tree === Rows(Seq(Window("window1"), Window("window2"), Window("window3")))
  }

  @Test
  def multiple_windows_in_first_row_and_one_window_in_second_row() = {
    /*
     -----
     | | |
     -----
     |   |
     -----
    */
    val tree = mkWindowTree(infos((0, 0), (0, 1), (1, 0)))
    tree === Rows(Seq(
        Columns(Seq(Window("window1"), Window("window2"))),
        Window("window3")))
  }

  @Test
  def single_window_in_first_row_and_multiple_windows_in_second_row() = {
    /*
     -----
     |   |
     -----
     | | |
     -----
    */
    val tree = mkWindowTree(infos((0, 0), (1, 0), (1, 1)))
    tree === Rows(Seq(
        Window("window1"),
        Columns(Seq(Window("window2"), Window("window3")))))
  }

  @Test
  def multiple_windows_in_all_rows() = {
    /*
     -----
     | | |
     -----
     | | |
     -----
    */
    val tree = mkWindowTree(infos((0, 0), (0, 1), (1, 0), (1, 1)))
    tree === Rows(Seq(
        Columns(Seq(Window("window1"), Window("window2"))),
        Columns(Seq(Window("window3"), Window("window4")))))
  }
}
