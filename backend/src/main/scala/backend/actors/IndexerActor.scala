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
      handleAskQuery(query, fmt)

    case AddFile(fileName, data) ⇒
      handleAddData(fileName, data)
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

  def handleAddData(fileName: String, data: Seq[Hierarchy]): Try[Unit] = {
    withDataset(IndexDataset) { dataset ⇒
      withModel(dataset, modelName)(add(modelName, fileName, data))
    }
  }
}

sealed trait IndexerMessage
object IndexerMessage {
  case class AskQuery(query: String, fmt: ResultsFormat) extends IndexerMessage
  case class AddFile(fileName: String, data: Seq[Hierarchy]) extends IndexerMessage
}
