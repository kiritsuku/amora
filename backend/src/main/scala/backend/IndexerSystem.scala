package backend

import research.indexer.hierarchy.Hierarchy
import research.indexer.Indexer
import scala.util.Try
import java.io.File

trait IndexerSystem {

  import Indexer._

  private val sep = System.getProperty("file.separator")

  /**
   * The location where we want to store our data. Since we can run the program
   * from different locations we need a stable location which never changes.
   * This method returns such a path.
   */
  val storageLocation: String = {
    val home = System.getProperty("user.home")
    val xdgPath = ".config"
    val dir = "tooling-research"

    val loc = s"$home$sep$xdgPath$sep$dir"
    new File(loc).mkdirs()
    loc
  }

  def addData(filename: String, data: Seq[Hierarchy]): Try[Unit] = {
    val modelName = "http://test.model/"

    withDataset(s"$storageLocation${sep}dataset") { dataset ⇒
      withModel(dataset, modelName)(add(modelName, filename, data))
    }
  }
}
