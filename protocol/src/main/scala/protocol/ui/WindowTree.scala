package protocol.ui

sealed trait WindowTree
case class Rows(rows: Seq[WindowTree]) extends WindowTree
case class Columns(columns: Seq[WindowTree]) extends WindowTree
case class Window(id: Int) extends WindowTree
