package research

import java.io.PrintWriter
import java.io.StringWriter

object Logger {

  sealed abstract class LogLevel(val value: Int)
  case object Debug extends LogLevel(0)
  case object Info extends LogLevel(1)
  case object Warning extends LogLevel(2)
  case object Error extends LogLevel(3)
}

final class Logger {
  import Logger._

  private val sw = new StringWriter
  private val pw = new PrintWriter(sw)

  private var level: LogLevel = Info

  def debug(msg: String): Unit = {
    if (level.value >= Debug.value) {
      pw.print("[debug] ")
      pw.println(msg)
    }
  }

  def warning(msg: String): Unit = {
    if (level.value >= Warning.value) {
      pw.print("[warning] ")
      pw.println(msg)
    }
  }

  def info(msg: String): Unit = {
    if (level.value >= Info.value) {
      pw.print("[info] ")
      pw.println(msg)
    }
  }

  def error(msg: String, t: Throwable): Unit = {
    if (level.value >= Error.value) {
      pw.print("[error] ")
      pw.println(msg)
      t.printStackTrace(pw)
    }
  }

  def log: String = sw.toString()
  def logLevel = level
  def logLevel_=(level: LogLevel) = this.level = level
}
