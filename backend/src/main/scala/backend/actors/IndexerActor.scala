package backend.actors

import java.io.ByteArrayOutputStream

import scala.util.Try

import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.sparql.resultset.ResultsFormat

import akka.actor.Actor
import backend.indexer.Indexer
import backend.indexer.IndexerConstants
import research.converter.protocol.Hierarchy

class IndexerActor extends Actor {

  import Indexer._
  import IndexerConstants._
  import IndexerMessage._

  private val modelName = "http://test.model/"

  override def receive = {
    case AskQuery(query, fmt) ⇒
      sender ! handleAskQuery(query, fmt)

    case AddData(data) ⇒
      handleAddData(data)
  }

  def handleAskQuery(query: String, fmt: ResultsFormat): Try[String] = {
    withDataset(IndexDataset) { dataset ⇒
      withModel(dataset, modelName) { model ⇒
        withQueryService(modelName, query)(model) map { r ⇒
          val s = new ByteArrayOutputStream

          ResultSetFormatter.output(s, r, fmt)
          new String(s.toByteArray(), "UTF-8")
        }
      }.flatten
    }.flatten
  }

  def handleAddData(data: Indexable): Try[Unit] = {
    withDataset(IndexDataset) { dataset ⇒
      withModel(dataset, modelName) { model ⇒
        data match {
          case p: Project ⇒ addProject(modelName, p)(model)
          case f: File ⇒ addFile(modelName, f)(model)
        }
      }
    }
  }
}

sealed trait IndexerMessage
object IndexerMessage {
  case class AskQuery(query: String, fmt: ResultsFormat) extends IndexerMessage
  case class AddData(data: Indexable) extends IndexerMessage

  sealed trait Indexable extends IndexerMessage
  final case class Artifact(organization: String, name: String, version: String) extends Indexable
  sealed trait Origin extends Indexable
  final case class Project(name: String, artifact: Artifact) extends Origin
  case object NoOrigin extends Origin
  final case class File(origin: Origin, name: String, data: Seq[Hierarchy]) extends Indexable
}
