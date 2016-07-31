package backend.actors

import scala.concurrent.Await
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import akka.actor.Actor
import akka.actor.ActorRef
import akka.pattern.ask
import backend.Logger
import backend.PlatformConstants

/**
 * Common functionality for all components that want to index data.
 */
trait DataIndexer { this: Actor ⇒

  def indexer: ActorRef
  def logger: Logger

  implicit def dispatcher = context.system.dispatcher

  def indexData(data: IndexerMessage.Indexable, errMsg: ⇒ String): Unit = {
    import PlatformConstants.timeout
    Await.ready((indexer ask IndexerMessage.AddData(data)).mapTo[Unit], timeout.duration) onComplete {
      case Failure(t) ⇒
        logger.error(errMsg, t)
      case _ ⇒
    }
  }

  def runIndexing(sender: ActorRef)(f: ⇒ Unit): Unit = {
    Future(f).onComplete {
      case Success(_) ⇒
        sender ! QueueMessage.Completed
      case Failure(f) ⇒
        throw f
    }
  }

}
