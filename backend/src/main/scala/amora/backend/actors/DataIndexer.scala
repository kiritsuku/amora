package amora.backend.actors

import scala.concurrent.Await
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import akka.actor.Actor
import akka.actor.ActorRef
import akka.pattern.ask
import amora.backend.Logger

/**
 * Common functionality for all components that want to index data.
 */
trait DataIndexer { this: Actor ⇒

  def indexer: ActorRef
  def logger: Logger

  implicit def dispatcher = context.system.dispatcher

  def sparqlUpdate(query: String, errMsg: ⇒ String): Unit = {
    import amora.backend.PlatformConstants.timeout
    Await.ready((indexer ask IndexerMessage.RunUpdate(query)).mapTo[Try[Unit]], timeout.duration) onComplete {
      case Failure(t) ⇒
        // TODO we can't log here anymore because logger is already closed
        logger.error(errMsg, t)
      case Success(Failure(t)) ⇒
        logger.error(errMsg, t)
      case Success(Success(())) ⇒
    }
  }

  def runIndexing(sender: ActorRef)(f: ⇒ Unit): Unit = {
    Future(f).onComplete {
      case Success(_) ⇒
        logger.close()
        sender ! QueueMessage.Completed
      case Failure(f) ⇒
        throw f
    }
  }

}
