package backend.actors

import scala.collection.mutable.Queue
import scala.concurrent.duration._
import scala.util.control.NonFatal

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import backend.Logger
import backend.ActorLogger

final class QueueActor extends Actor {
  import QueueMessage._
  private implicit val system = context.system
  import system.dispatcher
  private val log = context.system.log

  private val queue = Queue[Item]()
  private var cancellable = mkQueueRunner()
  private var id = 0
  private var running = false

  private val runner: ActorRef = system.actorOf(Props(new Actor {
    override def receive = {
      case item @ Item(id, func, logger) ⇒
        try func(logger) catch {
          case NonFatal(t) ⇒
            log.error(t, "Error happened during execution of queue item.")
        }
        running = false
    }
  }))

  override def receive = {
    case Add(func) ⇒
      val item = Item(genId, func, new ActorLogger)
      queue.enqueue(item)
      sender ! item.id
    case Stop ⇒
      cancellable.cancel()
    case Start ⇒
      if (cancellable.isCancelled)
        cancellable = mkQueueRunner()
    case Check ⇒
      if (!running && queue.nonEmpty) {
        val item = queue.dequeue()
        log.info(s"Queue scheduler handles item with id ${item.id}. ${queue.size} elements remaining.")
        running = true
        runner ! item
      }
    case GetItems ⇒
      sender ! queue.map(_.id)
    case GetItem(id) ⇒
      sender ! queue.find(_.id == id).map(_.logger)
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

sealed trait QueueMessage
object QueueMessage {
  case class Add(func: Logger ⇒ Unit) extends QueueMessage
  case object Check extends QueueMessage
  case object Stop extends QueueMessage
  case object Start extends QueueMessage
  case object GetItems extends QueueMessage
  case class GetItem(id: Int) extends QueueMessage
}
