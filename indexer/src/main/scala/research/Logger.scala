package research

import java.io.PrintWriter
import java.io.StringWriter

final class Logger {

  private val sw = new StringWriter
  private val pw = new PrintWriter(sw)

  def warning(msg: String): Unit = {
    pw.print("[warning] ")
    pw.println(msg)
  }

  def info(msg: String): Unit = {
    pw.print("[info] ")
    pw.println(msg)
  }

  def error(msg: String, t: Throwable): Unit = {
    pw.print("[error] ")
    pw.println(msg)
    t.printStackTrace(pw)
  }

  def log: String = sw.toString()
}
