package backend
package internal

import org.junit.Test

import protocol.ui.Columns
import protocol.ui.Rows
import protocol.ui.Window

class WindowTreeCreatorTest {
  import WindowTreeCreator._
  import TestUtils._

  private def infos(pos: (Int, Int)*): Seq[WinInfo] = {
    pos.zipWithIndex.toList map {
      case ((row, col), i) ⇒ WinInfo(i+1, row, col, 1, 1)
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
     -------
     | | | |
     -------
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
     | |
     ---
    */
    val tree = mkWindowTree(infos((0, 0), (1, 0), (2, 0)))
    tree === Rows(Seq(Window("window1"), Window("window2"), Window("window3")))
  }

  @Test
  def multiple_winodws_in_first_column_and_one_window_in_second_column() = {
    /*
     -----
     | | |
     --- |
     | | |
     -----
    */
    val tree = mkWindowTree(infos((0, 0), (1, 0), (0, 1)))
    tree === Columns(Seq(
        Rows(Seq(Window("window1"), Window("window2"))),
        Window("window3")))
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

  @Test
  def multiple_windows_nested_between_multiple_windows1() = {
    /*
     ---------
     |   |   |
     |   |   |
     |   |   |
     ---------
     |   | | |
     |   -----
     |   | | |
     ---------
    */
    val tree = mkWindowTree(infos((0, 0), (0, 1), (1, 0), (1, 1), (1, 2), (2, 1), (2, 2)))
    tree === Rows(Seq(
        Columns(Seq(Window("window1"), Window("window2"))),
        Columns(Seq(
            Window("window3"),
            Rows(Seq(
                Columns(Seq(Window("window4"), Window("window5"))),
                Columns(Seq(Window("window6"), Window("window7")))))))))
  }

  @Test
  def multiple_windows_nested_between_multiple_windows2() = {
    /*
     ---------
     |   |   |
     |   |   |
     |   |   |
     ---------
     | | |   |
     -----   |
     | | |   |
     ---------
    */
    val tree = mkWindowTree(infos((0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1), (1, 3)))
    tree === Rows(Seq(
        Columns(Seq(Window("window1"), Window("window2"))),
        Columns(Seq(
            Rows(Seq(
                Columns(Seq(Window("window3"), Window("window4"))),
                Columns(Seq(Window("window5"), Window("window6"))))),
            Window("window7")))))
  }

  @Test
  def unsorted_coordinates_should_be_handled_well() = {
    /*
     -----
     | | |
     -----
     | | |
     -----
    */
    val tree = mkWindowTree(infos((1, 0), (0, 1), (0, 0), (1, 1)))
    tree === Rows(Seq(
        Columns(Seq(Window("window3"), Window("window2"))),
        Columns(Seq(Window("window1"), Window("window4")))))
  }
}
