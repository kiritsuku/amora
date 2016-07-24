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

  import Indexer._
  import IndexerConstants._
  import IndexerMessage._

  override def receive = {
    case RunQuery(query) ⇒
      sender ! handleQuery(query)

    case AddData(data) ⇒
      handleAddData(data)
  }

  def handleQuery(query: String): Try[ResultSetRewindable] = {
    log.info(s"Handle SPARQL query: $query")
    withDataset(IndexDataset) { dataset ⇒
      withModel(dataset, Content.ModelName) { model ⇒
        withQueryService(model, query)
      }.flatten
    }.flatten
  }

  def handleAddData(data: Indexable): Try[Unit] = {
    withDataset(IndexDataset) { dataset ⇒
      withModel(dataset, Content.ModelName) { model ⇒
        add(Content.ModelName, model, data)
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
