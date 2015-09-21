package protocol.ui

sealed trait WindowTree
case class Rows(rows: Seq[WindowTree]) extends WindowTree
case class Columns(columns: Seq[WindowTree]) extends WindowTree
case class Window(divId: String) extends WindowTree
