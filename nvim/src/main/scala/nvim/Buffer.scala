package nvim

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import akka.actor.ActorSystem
import msgpack4z.MsgpackArray
import msgpack4z.MsgpackBinary
import msgpack4z.MsgpackLong
import msgpack4z.MsgpackUnion._

/**
 * Low level wrapper around Nvims msgpack-rpc protocol. Represents a Nvim
 * buffer; all operations operate directly on the buffer data structure of Nvim.
 */
final case class Buffer(id: Int, connection: Connection)(implicit system: ActorSystem) {

  /**
   * Returns the absolute path of the file that is represented by this buffer.
   */
  def name(implicit ec: ExecutionContext): Future[String] = {
    connection.sendRequest("buffer_get_name", int(id)) {
      case MsgpackBinary(bin) ⇒ new String(bin, "UTF-8")
    }
  }

  /**
   * The number of lines. The line count for empty buffers is 1.
   */
  def lineCount(implicit ec: ExecutionContext): Future[Int] = {
    connection.sendRequest("buffer_line_count", int(id)) {
      case MsgpackLong(long) ⇒ long.toInt
    }
  }

  /**
   * Returns the line at `index`, where line 1 is represented by index 0. If a
   * large range of files should be retrieved, it is more efficient to use
   * [[lineSlice]] instead.
   *
   * @example {{{
   * val fileContent = for {
   *   count ← buffer.lineCount
   *   lines ← Future.sequence(0 to count map buffer.lineAt)
   * } yield lines.mkString("\n")
   * }}}
   */
  def lineAt(index: Int)(implicit ec: ExecutionContext): Future[String] = {
    connection.sendRequest("buffer_get_line", int(id), int(index)) {
      case MsgpackBinary(bin) ⇒ new String(bin, "UTF-8")
    }
  }

  /**
   * Returns all the lines between `start` and `end`. With `includeStart` and
   * `includeEnd` it can be chosen whether `start` and `end` should be inclusive
   * or exclusive. The default parameters are set to inclusive for `start` and
   * exclusive for `end`.
   *
   * @example {{{
   * buffer.lineSlice(7, 10)
   * // this may return
   * // Seq("line7", "line8", "line9")
   * }}}
   */
  def lineSlice
      (start: Int, end: Int, includeStart: Boolean = true, includeEnd: Boolean = false)
      (implicit ec: ExecutionContext)
      : Future[Seq[String]] = {
    connection.sendRequest("buffer_get_line_slice", int(id), int(start), int(end), bool(includeStart), bool(includeEnd)) {
      case MsgpackArray(lines) ⇒
        if (!lines.forall(_.isInstanceOf[MsgpackBinary]))
          throw new UnexpectedResponse(lines.toString)
        else
          lines map { x => (x: @unchecked) match {
            case MsgpackBinary(bin) ⇒ new String(bin, "UTF-8")
          }}
    }
  }

}
