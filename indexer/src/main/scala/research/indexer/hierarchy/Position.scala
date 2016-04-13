package research.indexer.hierarchy

trait Position

final case class RangePosition(start: Int, end: Int) extends Position {
  def length = end-start
}

final case object NoPosition extends Position
