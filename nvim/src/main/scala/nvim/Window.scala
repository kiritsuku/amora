package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import msgpack4z._
import msgpack4z.MsgpackUnion._

case class Window(id: Int, connection: Connection) {

  /**
   * Returns the cursor of the window as a tuple of row and column. The row
   * starts at 1 and the column starts at 0.
   */
  def cursor(implicit ec: ExecutionContext): Future[Position] = {
    connection.sendRequest("window_get_cursor", int(id)) {
      case MsgpackArray(List(MsgpackLong(row), MsgpackLong(col))) ⇒
        Position(row.toInt, col.toInt)
    }
  }

  /**
   * Sets the cursor to `pos` and returns `pos` afterwards.
   */
  def cursor_=(pos: Position)(implicit ec: ExecutionContext): Future[Position] = {
    connection.sendRequest("window_set_cursor", int(id), array(List(int(pos.row), int(pos.col)))) {
      case _ ⇒ pos
    }
  }

  /**
   * Returns the buffer that is displayed in this window.
   */
  def buffer(implicit ec: ExecutionContext): Future[Buffer] = {
    connection.sendRequest("window_get_buffer", int(id)) {
      case MsgpackExt(Nvim.BufferId, Array(bufId)) ⇒
        Buffer(bufId.toInt, connection)
    }
  }

  /**
   * Returns the screen position of the window. Both the row and the col start
   * at zero.
   */
  def position(implicit ec: ExecutionContext): Future[Position] = {
    connection.sendRequest("window_get_position", int(id)) {
      case MsgpackArray(List(MsgpackLong(x), MsgpackLong(y))) ⇒
        Position(x.toInt, y.toInt)
    }
  }

  /**
   * Gets the height of the window. The height is measured in number of lines
   * that can be displayed by the window.
   */
  def height(implicit ec: ExecutionContext): Future[Int] = {
    connection.sendRequest("window_get_height", int(id)) {
      case MsgpackLong(long) ⇒ long.toInt
    }
  }

  /**
   * Sets the height of the window. The height is measured in number of lines
   * that can be displayed by the window. Returns the height when the request
   * has been completed.
   */
  def height_=(height: Int)(implicit ec: ExecutionContext): Future[Int] = {
    connection.sendRequest("window_set_height", int(id), int(height)) {
      case _ ⇒ height
    }
  }

  /**
   * Gets the width of the window. The width is measured in number of characters
   * that can be displayed by the window in a single line.
   */
  def width(implicit ec: ExecutionContext): Future[Int] = {
    connection.sendRequest("window_get_width", int(id)) {
      case MsgpackLong(long) ⇒ long.toInt
    }
  }

  /**
   * Sets the height of the window. The width is measured in number of characters
   * that can be displayed by the window in a single line. Returns the width
   * when the request has been completed.
   */
  def width_=(width: Int)(implicit ec: ExecutionContext): Future[Int] = {
    connection.sendRequest("window_set_width", int(id), int(width)) {
      case _ ⇒ width
    }
  }
}
