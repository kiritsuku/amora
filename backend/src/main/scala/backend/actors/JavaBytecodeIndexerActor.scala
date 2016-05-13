package backend.actors

import scala.concurrent.Future

import akka.actor.Actor
import akka.actor.ActorRef

import backend.Logger
import backend.indexer.JavaBytecodeIndexer

final class JavaBytecodeIndexerActor(indexer: ActorRef, logger: Logger) extends Actor  {

  implicit def dispatcher = context.system.dispatcher

  override def receive = {
    case RequestMessage.Files(_, files) ⇒
      val s = sender
      Future(handleJavaBytecode(files, new JavaBytecodeIndexer(logger))).onComplete {
        case scala.util.Success(_) ⇒ s ! QueueMessage.Completed
        case scala.util.Failure(f) ⇒ throw f
      }
    case RequestMessage.GetLogger ⇒
      sender ! logger
  }

  private def handleJavaBytecode(files: Seq[RequestMessage.File], indexer: JavaBytecodeIndexer) = {
    val res = indexer.bytecodeToHierarchy(files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        this.indexer ! IndexerMessage.AddFile(fileName, hierarchy)
    }
  }

}
