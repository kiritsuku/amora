package amora.backend.actors

import akka.actor.Actor
import akka.actor.ActorRef
import amora.backend.Logger
import amora.backend.indexer.JavaBytecodeIndexer
import amora.backend.schema._
import amora.converter.protocol._

final class JavaBytecodeIndexerActor(override val indexer: ActorRef, override val logger: Logger)
    extends Actor with DataIndexer  {

  override def receive = {
    case RequestMessage.Files(_, files) ⇒
      runIndexing(sender) {
        handleJavaBytecode(Artifact(Project("testProject"), "o", "n", "v1"), files, new JavaBytecodeIndexer(logger))
      }
    case RequestMessage.GetLogger ⇒
      sender ! logger
  }

  private def handleJavaBytecode(origin: Schema, files: Seq[RequestMessage.File], indexer: JavaBytecodeIndexer) = {
    val res = indexer.bytecodeToHierarchy(files.map{ f ⇒ f.fileName → f.src }).get
    res foreach {
      case (fileName, hierarchy) ⇒
        logger.info(s"Indexing ${hierarchy.size} entries of file $fileName")
        def asSchemaPackage(decl: Hierarchy): Schema = decl match {
          case Root ⇒ origin
          case Decl(name, owner) ⇒ Package(name, asSchemaPackage(owner))
          case _ ⇒ ???
        }
        val pkg = hierarchy.collectFirst {
          case d if d.attachments(Attachment.Package) ⇒ d
        }
        val s = pkg.map(asSchemaPackage).map(pkg ⇒ File(pkg, fileName)).getOrElse(File(origin, fileName))
        sparqlUpdate(Schema.mkSparqlUpdate(Seq(s)), s"Error happened while indexing $fileName.")
        sparqlUpdate(Schema.mkSparqlUpdate(s, hierarchy), s"Error happened while indexing $fileName.")
    }
  }

}
