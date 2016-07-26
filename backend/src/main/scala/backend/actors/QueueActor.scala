package backend.actors

import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.concurrent.duration._

import akka.actor.Actor
import akka.actor.ActorRef
import akka.pattern.ask
import backend.PlatformConstants

final class QueueActor extends Actor {
  import QueueMessage._
  private implicit val system = context.system
  import system.dispatcher
  private val log = context.system.log

  private val queue = Queue[Item]()
  private var cancellable = mkQueueRunner()
  private var id = 0
  private var running = false
  private val history = Map[Int, Item]()

  override def receive = {
    case RunWithData(actor, data) ⇒
      val item = Item(genId, actor, data)
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
        history += item.id → item
        log.info(s"Queue scheduler handles item with id ${item.id}. ${queue.size} elements remaining.")
        running = true
        import PlatformConstants.timeout
        item.actor ask item.data onComplete { v ⇒
          running = false
          v match {
            case util.Success(Completed) ⇒ log.info(s"Processing queue item with id ${item.id} finished successfully.")
            case util.Success(msg) ⇒ log.error(s"Processing queue item with id ${item.id} finished successfully but it sent an unexpected message: $msg")
            case util.Failure(f) ⇒ log.error(f, s"Processing queue item with id ${item.id} failed.")
          }
        }
      }
    case GetItems ⇒
      sender ! queue.map(_.id) ++ history.keys
    case GetItem(id) ⇒
      sender ! queue.find(_.id == id).orElse(history.get(id)).map(_.actor)
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

  private case class Item(id: Int, actor: ActorRef, data: Any)
}

sealed trait QueueMessage
object QueueMessage {
  case class RunWithData(actor: ActorRef, data: Any) extends QueueMessage
  case object Check extends QueueMessage
  case object Stop extends QueueMessage
  case object Start extends QueueMessage
  case object GetItems extends QueueMessage
  case class GetItem(id: Int) extends QueueMessage
  case object Completed extends QueueMessage
}
