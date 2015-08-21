package nvim

import akka.actor.ActorSystem

case class Window(id: Int, connection: Connection)(implicit system: ActorSystem) {

}
