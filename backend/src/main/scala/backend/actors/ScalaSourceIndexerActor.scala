package backend.actors

import akka.actor.Actor
import akka.actor.ActorRef
import backend.Logger
import backend.indexer.ScalaSourceIndexer
import backend.schema._
import research.converter.protocol._

final class ScalaSourceIndexerActor(override val indexer: ActorRef, override val logger: Logger)
    extends Actor with DataIndexer {

  override def receive = {
    case RequestMessage.Files(_, files) ⇒
      runIndexing(sender) {
        handleScalaSource(Artifact(Project("testProject"), "o", "n", "v1"), files, new ScalaSourceIndexer(logger))
      }
    case RequestMessage.GetLogger ⇒
      sender ! logger
  }

  private def handleScalaSource(origin: Schema, files: Seq[RequestMessage.File], indexer: ScalaSourceIndexer): Unit = {
    val res = indexer.convertToHierarchy(files.map{ f ⇒ f.fileName → f.src }).get
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
        sparqlUpdate(HierarchySchema.mkSparqlUpdate(s, hierarchy), s"Error happened while indexing $fileName.")
    }
  }
}
