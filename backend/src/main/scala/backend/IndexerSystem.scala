package backend

import java.io.ByteArrayOutputStream

import scala.util.Try

import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.sparql.resultset.ResultsFormat

import backend.indexer.Indexer
import backend.indexer.IndexerConstants
import research.converter.protocol.Hierarchy

trait IndexerSystem {

  import Indexer._
  import IndexerConstants._

  private val modelName = "http://test.model/"

  def askQuery(query: String, fmt: ResultsFormat): Try[String] = {
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

  def addData(filename: String, data: Seq[Hierarchy]): Try[Unit] = {
    withDataset(IndexDataset) { dataset ⇒
      withModel(dataset, modelName)(add(modelName, filename, data))
    }
  }
}
