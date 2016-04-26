package backend

import java.io.PrintWriter
import java.io.StringWriter

import scala.collection.mutable.ListBuffer

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Source

object Logger {
  sealed abstract class LogLevel(val value: Int)
  case object Debug extends LogLevel(3)
  case object Info extends LogLevel(2)
  case object Warning extends LogLevel(1)
  case object Error extends LogLevel(0)
}

trait Logger {
  def debug(msg: String): Unit
  def warning(msg: String): Unit
  def info(msg: String): Unit
  def error(msg: String, t: Throwable): Unit
  def log: Source[String, Unit]
  def logLevel: Logger.LogLevel
  def logLevel_=(level: Logger.LogLevel): Unit
}

final class ActorLogger(implicit val system: ActorSystem) extends Logger {
  import Logger._

  private var level: LogLevel = Info
  val sources = ListBuffer[ActorRef]()
  val sw = new StringWriter
  val pw = new PrintWriter(sw)

  override def debug(msg: String): Unit = {
    if (level.value >= Debug.value) {
      val m = s"[debug] $msg\n"
      sources foreach (_ ! m)
    }
  }

  override def warning(msg: String): Unit = {
    if (level.value >= Warning.value) {
      val m = s"[warning] $msg\n"
      sources foreach (_ ! m)
    }
  }

  override def info(msg: String): Unit = {
    if (level.value >= Info.value) {
      val m = s"[info] $msg\n"
      sources foreach (_ ! m)
    }
  }

  override def error(msg: String, t: Throwable): Unit = {
    if (level.value >= Error.value) {
      val sw = new StringWriter
      val pw = new PrintWriter(sw)
      t.printStackTrace(pw)

      val m = s"[error] $msg\n${sw.toString}\n"
      sources foreach (_ ! m)
    }
  }

  override def log: Source[String, Unit] =
    Source.actorRef[String](10, OverflowStrategy.fail).mapMaterializedValue { ref ⇒
      sources += ref
      ref ! sw.toString
    }

  override def logLevel: LogLevel = level
  override def logLevel_=(level: LogLevel): Unit = this.level = level
}

case object IgnoreLogger extends Logger {
  override def debug(msg: String): Unit = ()
  override def warning(msg: String): Unit = ()
  override def info(msg: String): Unit = ()
  override def error(msg: String, t: Throwable): Unit = ()
  override def log: Source[String, Unit] = throw new UnsupportedOperationException
  override def logLevel: Logger.LogLevel = throw new UnsupportedOperationException
  override def logLevel_=(level: Logger.LogLevel): Unit = throw new UnsupportedOperationException
}
