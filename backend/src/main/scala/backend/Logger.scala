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

final class Logger(implicit val system: ActorSystem) {
  import Logger._

  private var level: LogLevel = Info
  val sources = ListBuffer[ActorRef]()
  val sw = new StringWriter
  val pw = new PrintWriter(sw)

  def debug(msg: String): Unit = {
    if (level.value >= Debug.value) {
      val m = s"[debug] $msg\n"
      sources foreach (_ ! m)
    }
  }

  def warning(msg: String): Unit = {
    if (level.value >= Warning.value) {
      val m = s"[warning] $msg\n"
      sources foreach (_ ! m)
    }
  }

  def info(msg: String): Unit = {
    if (level.value >= Info.value) {
      val m = s"[info] $msg\n"
      sources foreach (_ ! m)
    }
  }

  def error(msg: String, t: Throwable): Unit = {
    if (level.value >= Error.value) {
      val sw = new StringWriter
      val pw = new PrintWriter(sw)
      t.printStackTrace(pw)

      val m = s"[error] $msg\n${sw.toString}\n"
      sources foreach (_ ! m)
    }
  }

  def log: Source[String, Unit] =
    Source.actorRef[String](10, OverflowStrategy.fail).mapMaterializedValue { ref ⇒
      sources += ref
      ref ! sw.toString
    }

  def logLevel: LogLevel = level
  def logLevel_=(level: LogLevel): Unit = this.level = level
}
