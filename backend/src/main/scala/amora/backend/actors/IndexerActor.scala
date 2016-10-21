package amora.backend.actors

import scala.util.Try

import org.apache.jena.query.ResultSetRewindable

import akka.actor.Actor
import akka.actor.ActorLogging
import amora.backend.Content
import amora.backend.indexer.Indexer

class IndexerActor extends Actor with ActorLogging {

  import IndexerMessage._

  private val indexer = new Indexer(Content.ModelName)
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
      sender ! Try(handleQuery(query))

    case RunUpdate(query) ⇒
      sender ! Try(handleUpdate(query))

    case RunTurtleUpdate(query) ⇒
      sender ! Try(handleTurtleUpdate(query))
  }

  override def postStop() = {
    dataset.close()
  }

  def handleQuery(query: String): ResultSetRewindable = {
    log.info(s"Handle SPARQL query:\n$query")
    indexer.readDataset(dataset) { dataset ⇒
      indexer.withModel(dataset) { model ⇒
        indexer.withQueryService(model, query)
      }
    }
  }

  def handleUpdate(query: String): Unit = {
    log.info(s"Handle SPARQL update:\n$query")
    indexer.writeDataset(dataset) { dataset ⇒
      indexer.withModel(dataset) { model ⇒
        indexer.withUpdateService(model, query)(_ ⇒ ())
      }
    }
  }

  def handleTurtleUpdate(query: String): Unit = {
    log.info(s"Handle Turtle update:\n$query")
    indexer.writeDataset(dataset) { dataset ⇒
      indexer.withModel(dataset) { model ⇒
        indexer.addTurtle(model, query)
      }
    }
  }
}

sealed trait IndexerMessage
object IndexerMessage {
  case class RunQuery(query: String) extends IndexerMessage
  case class RunUpdate(query: String) extends IndexerMessage
  case class RunTurtleUpdate(query: String) extends IndexerMessage
}
