package amora.backend.indexer

import java.io.{ File ⇒ JFile }

import akka.actor.Actor
import akka.actor.ActorRef
import amora.backend.Logger
import amora.backend.actors.DataIndexer
import amora.backend.actors.RequestMessage
import amora.converter.protocol._

final class ArtifactIndexer(override val indexer: ActorRef, override val logger: Logger)
    extends Actor with DataIndexer with ArtifactFetcher {

  override def receive = {
    case RequestMessage.Artifacts(_, artifacts) ⇒
      runIndexing(sender) {
        val as = artifacts map { a =>
          Artifact(Project(a.name), a.organization, a.name, a.version)
        }
        handleArtifacts(as)
      }
    case RequestMessage.GetLogger ⇒
      sender ! logger
  }

  override def cacheLocation = new JFile(context.system.settings.config.getString("app.storage.artifact-repo"))
}
