package backend.actors

import akka.actor.Actor
import akka.actor.ActorRef

import backend.Logger
import backend.indexer.ScalaSourceIndexer

final class ScalaSourceIndexerActor(override val indexer: ActorRef, override val logger: Logger)
    extends Actor with DataIndexer {

  override def receive = {
    case RequestMessage.Files(_, files) ⇒
      runIndexing(sender) {
        handleScalaSource(files, new ScalaSourceIndexer(logger))
      }
    case RequestMessage.GetLogger ⇒
      sender ! logger
  }

  private def handleScalaSource(files: Seq[RequestMessage.File], indexer: ScalaSourceIndexer) = {
    val res = indexer.convertToHierarchy(files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        logger.info(s"Indexing ${hierarchy.size} entries of file $fileName")
        indexData(IndexerMessage.File(IndexerMessage.NoOrigin, fileName, hierarchy), s"Error happened while indexing $fileName.")
    }
  }

}
