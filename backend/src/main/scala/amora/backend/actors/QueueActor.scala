package amora.backend.actors

import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.concurrent.duration._

import akka.actor.Actor
import akka.actor.ActorRef

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
  private var processedItem: Item = _

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
        processedItem = queue.dequeue()
        history += processedItem.id → processedItem
        log.info(s"Queue scheduler handles item with id ${processedItem.id}. ${queue.size} elements remaining.")
        running = true
        processedItem.actor ! processedItem.data
      }
    case Completed ⇒
      running = false
      log.info(s"Processing queue item with id ${processedItem.id} finished successfully.")
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
