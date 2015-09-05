package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import akka.actor.ActorSystem
import msgpack4z._
import msgpack4z.MsgpackUnion._

case class Window(id: Int, connection: Connection)(implicit system: ActorSystem) {

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
   * Sets the cursor to `pos`.
   */
  def cursor_=(pos: Position)(implicit ec: ExecutionContext): Future[Unit] = {
    connection.sendRequest("window_set_cursor", int(id), array(List(int(pos.row), int(pos.col)))) {
      case _ ⇒ ()
    }
  }

  /**
   * Returns the buffer that is displayed in this window.
   */
  def buffer(implicit ec: ExecutionContext): Future[Buffer] = {
    connection.sendRequest("window_get_buffer", int(id)) {
      case MsgpackExt(Nvim.BufferId, MsgpackBinary(bin)) ⇒
        val bufId = bin.head.toInt
        Buffer(bufId, connection)
    }
  }
}
