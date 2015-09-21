package backend.internal

import org.scalatest.FunSuite

class WindowTreeTest extends FunSuite {

  import WindowTreeCreator._

  private def infos(pos: (Int, Int)*): Seq[WinInfo] = {
    pos.zipWithIndex map {
      case ((row, col), i) ⇒ WinInfo(i+1, row, col)
    }
  }

  test("single window") {
    /*
     ---
     | |
     ---
    */
    val tree = mkWindowTree(infos((0, 0)))
    assert(tree == Window("window1"))
  }

  test("multiple windows in a single row") {
    /*
     -----
     | | |
     -----
    */
    val tree = mkWindowTree(infos((0, 0), (0, 1), (0, 2)))
    assert(tree == Columns(Seq(Window("window1"), Window("window2"), Window("window3"))))
  }

  test("multiple windows in a single column") {
    /*
     ---
     | |
     ---
     | |
     ---
    */
    val tree = mkWindowTree(infos((0, 0), (1, 0), (2, 0)))
    assert(tree == Rows(Seq(Window("window1"), Window("window2"), Window("window3"))))
  }

  test("multiple windows in first row and one window in second row") {
    /*
     -----
     | | |
     -----
     |   |
     -----
    */
    val tree = mkWindowTree(infos((0, 0), (0, 1), (1, 0)))
    assert(tree == Rows(Seq(
        Columns(Seq(Window("window1"), Window("window2"))),
        Window("window3"))))
  }

  test("single window in first row and multiple windows in second row") {
    /*
     -----
     |   |
     -----
     | | |
     -----
    */
    val tree = mkWindowTree(infos((0, 0), (1, 0), (1, 1)))
    assert(tree == Rows(Seq(
        Window("window1"),
        Columns(Seq(Window("window2"), Window("window3"))))))
  }

  test("multiple windows in all rows") {
    /*
     -----
     | | |
     -----
     | | |
     -----
    */
    val tree = mkWindowTree(infos((0, 0), (0, 1), (1, 0), (1, 1)))
    assert(tree == Rows(Seq(
        Columns(Seq(Window("window1"), Window("window2"))),
        Columns(Seq(Window("window3"), Window("window4"))))))
  }
}
