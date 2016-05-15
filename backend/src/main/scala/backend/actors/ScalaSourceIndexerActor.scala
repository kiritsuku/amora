package backend.actors

import scala.concurrent.Future

import akka.actor.Actor
import akka.actor.ActorRef

import backend.Logger
import backend.indexer.ScalaSourceIndexer

final class ScalaSourceIndexerActor(indexer: ActorRef, logger: Logger) extends Actor {

  implicit def dispatcher = context.system.dispatcher

  override def receive = {
    case RequestMessage.Files(_, files) ⇒
      val s = sender
      Future(handleScalaSource(files, new ScalaSourceIndexer(logger))).onComplete {
        case scala.util.Success(_) ⇒ s ! QueueMessage.Completed
        case scala.util.Failure(f) ⇒ throw f
      }
    case RequestMessage.GetLogger ⇒
      sender ! logger
  }

  private def handleScalaSource(files: Seq[RequestMessage.File], indexer: ScalaSourceIndexer) = {
    val res = indexer.convertToHierarchy(files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        logger.info(s"Indexing ${hierarchy.size} entries of file $fileName")
        this.indexer ! IndexerMessage.AddData(IndexerMessage.File(IndexerMessage.NoOrigin, fileName, hierarchy))
    }
  }

}
