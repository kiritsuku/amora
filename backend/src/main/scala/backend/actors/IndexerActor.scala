package backend.actors

import org.apache.jena.query.ResultSetRewindable

import akka.actor.Actor
import akka.actor.ActorLogging
import backend.Content
import backend.indexer.Indexer
import research.converter.protocol.Hierarchy

class IndexerActor extends Actor with ActorLogging {

  import IndexerMessage._

  private val indexer = new Indexer
  private val config = context.system.settings.config
  private val testMode = config.getBoolean("app.test-mode")
  private val dataset =
    if (testMode)
      indexer.mkInMemoryDataset
    else
      indexer.mkDataset(config.getString("app.storage.index-dataset"))

  log.info("Indexer created dataset at: " + (if (testMode) "<memory>" else config.getString("app.storage.index-dataset")))
  indexer.writeDataset(dataset)(indexer.startupIndexer)

  override def receive = {
    case RunQuery(query) ⇒
      sender ! handleQuery(query)

    case AddData(data) ⇒
      sender ! handleAddData(data)
  }

  def handleQuery(query: String): ResultSetRewindable = {
    log.info(s"Handle SPARQL query: $query")
    indexer.writeDataset(dataset) { dataset ⇒
      indexer.withModel(dataset, Content.ModelName) { model ⇒
        indexer.withQueryService(model, query)
      }
    }
  }

  def handleAddData(data: Indexable): Unit = {
    indexer.writeDataset(dataset) { dataset ⇒
      indexer.withModel(dataset, Content.ModelName) { model ⇒
        indexer.add(Content.ModelName, model, data)
      }
    }
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
