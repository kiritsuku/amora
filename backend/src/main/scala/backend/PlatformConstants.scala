package backend

import scala.concurrent.duration._

import akka.util.Timeout

object PlatformConstants {

  implicit val timeout = Timeout(5.seconds)
}
