package backend.actors

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.UnhandledMessage

/**
 * Logs unhandled messages. The usage of this actor is better because the config
 * value `akka.actor.debug.unhandled = on` can only log at debug level.
 */
class UnhandledMessagesActor extends Actor with ActorLogging {
  override def receive = {
    case UnhandledMessage(msg, sender, recipient) ⇒
      log.error(s"Unhandled message `$msg` to `$recipient` from `$sender`.")
  }
}
