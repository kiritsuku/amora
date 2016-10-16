package amora.backend

import java.io.PrintWriter
import java.io.StringWriter

import scala.collection.mutable.ListBuffer

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.event.LogSource
import akka.event.Logging
import akka.event.LoggingAdapter
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

  def close(): Unit
  def isClosed: Boolean
}

final class ActorLogger(implicit val system: ActorSystem) extends Logger {
  import Logger._

  private lazy val akkaLogger: LoggingAdapter = {
    implicit val logSource: LogSource[AnyRef] = new LogSource[AnyRef] {
      override def genString(o: AnyRef): String = o.getClass.getName
      override def getClazz(o: AnyRef): Class[_] = o.getClass
    }
    Logging(system, this)
  }

  private val forwardLogger = system.settings.config.getBoolean("app.forward-internal-logger-to-akka-logger")
  private var level: LogLevel = Info
  private var _isClosed = false
  private val sources = ListBuffer[ActorRef]()
  private val sw = new StringWriter
  private val pw = new PrintWriter(sw)

  override def debug(msg: String): Unit = {
    checkIfClosed()

    if (level.value >= Debug.value) {
      val m = s"[debug] $msg\n"
      pw.append(m)
      sources foreach (_ ! m)
      if (forwardLogger)
        akkaLogger.debug(msg)
    }
  }

  override def warning(msg: String): Unit = {
    checkIfClosed()

    if (level.value >= Warning.value) {
      val m = s"[warning] $msg\n"
      pw.append(m)
      sources foreach (_ ! m)
      if (forwardLogger)
        akkaLogger.warning(msg)
    }
  }

  override def info(msg: String): Unit = {
    checkIfClosed()

    if (level.value >= Info.value) {
      val m = s"[info] $msg\n"
      pw.append(m)
      sources foreach (_ ! m)
      if (forwardLogger)
        akkaLogger.info(msg)
    }
  }

  override def error(msg: String, t: Throwable): Unit = {
    checkIfClosed()

    if (level.value >= Error.value) {
      val sw = new StringWriter
      val pw = new PrintWriter(sw)
      t.printStackTrace(pw)

      val m = s"[error] $msg\n${sw.toString}\n"
      pw.append(m)
      sources foreach (_ ! m)
      if (forwardLogger)
        akkaLogger.error(t, msg)
    }
  }

  override def log: Source[String, Unit] =
    Source.actorRef[String](10, OverflowStrategy.fail).mapMaterializedValue { ref ⇒
      sources += ref
      ref ! sw.toString
    }

  override def logLevel: LogLevel = level
  override def logLevel_=(level: LogLevel): Unit = this.level = level

  override def close() = {
    _isClosed = true

    sources foreach { ref ⇒
      ref ! "Logger successfully closed."
    }
  }

  override def isClosed = _isClosed

  private def checkIfClosed(): Unit =
    require(!isClosed, "Logger is closed. It is no longer possible to log anything.")
}

case object IgnoreLogger extends Logger {
  override def debug(msg: String): Unit = ()
  override def warning(msg: String): Unit = ()
  override def info(msg: String): Unit = ()
  override def error(msg: String, t: Throwable): Unit = ()
  override def log: Source[String, Unit] = throw new UnsupportedOperationException
  override def logLevel: Logger.LogLevel = throw new UnsupportedOperationException
  override def logLevel_=(level: Logger.LogLevel): Unit = throw new UnsupportedOperationException

  override def close() = throw new UnsupportedOperationException
  override def isClosed = false
}
