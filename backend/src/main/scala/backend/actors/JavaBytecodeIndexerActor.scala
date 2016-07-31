package backend.actors

import akka.actor.Actor
import akka.actor.ActorRef

import backend.Logger
import backend.indexer.JavaBytecodeIndexer

final class JavaBytecodeIndexerActor(override val indexer: ActorRef, override val logger: Logger)
    extends Actor with DataIndexer  {

  override def receive = {
    case RequestMessage.Files(_, files) ⇒
      runIndexing(sender) {
        handleJavaBytecode(files, new JavaBytecodeIndexer(logger))
      }
    case RequestMessage.GetLogger ⇒
      sender ! logger
  }

  private def handleJavaBytecode(files: Seq[RequestMessage.File], indexer: JavaBytecodeIndexer) = {
    val res = indexer.bytecodeToHierarchy(files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        logger.info(s"Indexing ${hierarchy.size} entries of file $fileName")
        indexData(IndexerMessage.File(IndexerMessage.NoOrigin, fileName, hierarchy), s"Error happened while indexing $fileName.")
    }
  }

}
