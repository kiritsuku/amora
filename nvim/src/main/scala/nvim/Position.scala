package nvim

final case class Position(row: Int, col: Int) {
  override def toString = s"[row=$row,col=$col]"
}
