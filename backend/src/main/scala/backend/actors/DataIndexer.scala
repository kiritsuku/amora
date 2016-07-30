package backend.actors

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.util.Failure

import akka.actor.ActorRef
import akka.pattern.ask
import backend.Logger
import backend.PlatformConstants

/**
 * Common functionality for all components that want to index data.
 */
trait DataIndexer {

  def indexer: ActorRef
  def logger: Logger

  def indexData(data: IndexerMessage.Indexable, errMsg: ⇒ String)(implicit ctx: ExecutionContext): Unit = {
    import PlatformConstants.timeout
    Await.ready((indexer ask IndexerMessage.AddData(data)).mapTo[Unit], timeout.duration) onComplete {
      case Failure(t) ⇒
        logger.error(errMsg, t)
      case _ ⇒
    }
  }

}
