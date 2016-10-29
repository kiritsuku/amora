package amora.backend.services

import java.io.File

import akka.actor.ActorSystem
import amora.backend.Logger
import amora.backend.indexer.ArtifactFetcher
import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

class IndexArtifacts(val system: ActorSystem, override val logger: Logger) extends ScalaService with ArtifactFetcher {
  import amora.api._

  override def cacheLocation = new File(system.settings.config.getString("app.storage.artifact-repo"))

  def run(turtleReq: String): String = {
    val artifacts = sparqlQuery"""
      prefix p:<http://amora.center/kb/amora/Schema/Project/>
      prefix a:<http://amora.center/kb/amora/Schema/Artifact/>
      select * where {
        [a a:] a:owner [p:name ?pname] ; a:organization ?org ; a:name ?name ; a:version ?version .
      }
    """.runOnModel(turtleModel(turtleReq)).map { row ⇒
      Artifact(Project(row.string("pname")), row.string("org"), row.string("name"), row.string("version"))
    }

    downloadAndIndexArtifacts(artifacts)

    response(s"""
      @prefix service:<http://amora.center/kb/Schema/Service/> .
      @prefix response:<http://amora.center/kb/ServiceResponse/> .
      <#this>
        a response: ;
        service:requestId <$requestId> ;
      .
    """)
  }
}
