package ui

import protocol.WindowUpdate
import protocol.Pos
import utest._

object WindowTreeTest extends TestSuite {
  import WindowTreeCreator._

  private def infos(pos: (Int, Int)*): Seq[WinInfo] = {
    pos.zipWithIndex map {
      case ((row, col), i) ⇒ WinInfo(i+1, row, col)
    }
  }

  def tests = TestSuite {
    "single window" - {
      /*
       ---
       | |
       ---
       */
      val tree = mkWindowTree(infos((0, 0)))
      assert(tree == Rows(Seq(Window("window1"))))
    }

    "multiple windows in a single row" - {
      /*
       -----
       | | |
       -----
       */
      val tree = mkWindowTree(infos((0, 0), (0, 1), (0, 2)))
      assert(tree == Rows(Seq(Columns(Seq(Window("window1"), Window("window2"), Window("window3"))))))
    }

    "multiple windows in a single column" - {
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

  }
}
