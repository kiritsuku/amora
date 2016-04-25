package backend.actors

import scala.collection.mutable.Queue
import scala.concurrent.duration._
import scala.util.control.NonFatal

import akka.actor.Actor
import research.Logger

final class QueueActor extends Actor {
  import QueueMsg._
  private implicit val d = context.system.dispatcher
  private val log = context.system.log

  private val queue = Queue[Item]()
  private var cancellable = mkQueueRunner()
  private var id = 0

  override def receive = {
    case Add(func) ⇒
      val item = Item(genId, func, new Logger)
      queue.enqueue(item)
      sender ! item.id
    case Stop ⇒
      cancellable.cancel()
    case Start ⇒
      if (cancellable.isCancelled)
        cancellable = mkQueueRunner()
    case Check ⇒
      if (queue.nonEmpty) {
        val Item(id, func, logger) = queue.dequeue()
        log.info(s"Queue scheduler handles item with id $id. ${queue.size} elements remaining.")
        try func(logger) catch {
          case NonFatal(t) ⇒
            log.error(t, "Error happened during execution of queue item.")
        }
        log.info(logger.log)
      }
  }

  /**
   * Periodically sends a [[Check]] message to the actor that tells it to handle
   * the next item in the queue if one exists.
   */
  private def mkQueueRunner() =
    context.system.scheduler.schedule(10.millis, 10.millis) {
      self ! Check
    }

  private def genId() = {
    id += 1
    id
  }

  private case class Item(id: Int, func: Logger ⇒ Unit, logger: Logger)
}

sealed trait QueueMsg
object QueueMsg {
  case class Add(func: Logger ⇒ Unit) extends QueueMsg
  case object Check extends QueueMsg
  case object Stop extends QueueMsg
  case object Start extends QueueMsg
}
