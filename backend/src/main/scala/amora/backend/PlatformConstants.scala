package amora.backend

import scala.concurrent.duration._

import akka.util.Timeout

object PlatformConstants {

  implicit val timeout = Timeout(5.seconds)

  /**
   * Returns `true` if the current JVM process runs in debug mode, `false`
   * otherwise.
   */
  val runsInDebugMode: Boolean =
    java.lang.management.ManagementFactory.getRuntimeMXBean.getInputArguments.toString.contains("-agentlib:jdwp")
}
