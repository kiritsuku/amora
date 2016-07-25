package backend.actors

import scala.util.Try

import org.apache.jena.query.ResultSetRewindable

import akka.actor.Actor
import akka.actor.ActorLogging
import backend.Content
import backend.indexer.Indexer
import backend.indexer.IndexerConstants
import research.converter.protocol.Hierarchy

class IndexerActor extends Actor with ActorLogging {

  import IndexerConstants._
  import IndexerMessage._

  private val indexer = new Indexer
  private val testMode = context.system.settings.config.getBoolean("app.test-mode")
  private val dataset =
    if (testMode)
      indexer.mkInMemoryDataset
    else
      indexer.mkDataset(IndexDataset)

  log.info("Indexer created dataset at: " + (if (testMode) "<memory>" else IndexDataset))
  indexer.withDataset(dataset)(indexer.startupIndexer)

  override def receive = {
    case RunQuery(query) ⇒
      sender ! handleQuery(query)

    case AddData(data) ⇒
      handleAddData(data)
  }

  def handleQuery(query: String): Try[ResultSetRewindable] = {
    log.info(s"Handle SPARQL query: $query")
    indexer.withDataset(dataset) { dataset ⇒
      indexer.withModel(dataset, Content.ModelName) { model ⇒
        indexer.withQueryService(model, query)
      }.flatten
    }.flatten
  }

  def handleAddData(data: Indexable): Try[Unit] = {
    indexer.withDataset(dataset) { dataset ⇒
      indexer.withModel(dataset, Content.ModelName) { model ⇒
        indexer.add(Content.ModelName, model, data)
      }
    }.flatten
  }
}

sealed trait IndexerMessage
object IndexerMessage {
  case class RunQuery(query: String) extends IndexerMessage
  case class AddData(data: Indexable) extends IndexerMessage

  sealed trait Indexable extends IndexerMessage
  sealed trait Origin extends Indexable
  final case class Artifact(project: Project, organization: String, name: String, version: String) extends Origin
  case object NoOrigin extends Origin
  final case class Project(name: String) extends Indexable
  final case class File(origin: Origin, name: String, data: Seq[Hierarchy]) extends Indexable
}
