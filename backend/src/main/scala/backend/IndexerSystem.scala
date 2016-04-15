package backend

import research.indexer.hierarchy.Hierarchy
import research.indexer.Indexer
import scala.util.Try
import java.io.File
import java.io.ByteArrayOutputStream
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.sparql.resultset.ResultsFormat

trait IndexerSystem {

  import Indexer._

  private val sep = System.getProperty("file.separator")

  /**
   * The location where we want to store our data. Since we can run the program
   * from different locations we need a stable location which never changes.
   * This method returns such a path.
   */
  private val storageLocation: String = {
    val home = System.getProperty("user.home")
    val xdgPath = ".config"
    val dir = "tooling-research"

    val loc = s"$home$sep$xdgPath$sep$dir"
    new File(loc).mkdirs()
    loc
  }

  private val dataset = s"$storageLocation${sep}dataset"

  private val modelName = "http://test.model/"

  def askQuery(query: String, fmt: ResultsFormat): Try[String] = {
    withDataset(dataset) { dataset ⇒
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
    withDataset(dataset) { dataset ⇒
      withModel(dataset, modelName)(add(modelName, filename, data))
    }
  }
}
