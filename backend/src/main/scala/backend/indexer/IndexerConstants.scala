package backend.indexer

import java.io.File

object IndexerConstants {

  private val sep = System.getProperty("file.separator")

  /**
   * The location where we want to store our data. Since we can run the program
   * from different locations we need a stable location which never changes.
   * This method returns such a path.
   */
  val StorageLocation: String = {
    val home = System.getProperty("user.home")
    val xdgPath = ".config"
    val dir = "tooling-research"

    val loc = s"$home$sep$xdgPath$sep$dir"
    new File(loc).mkdirs()
    loc
  }

  val IndexDataset = s"$StorageLocation${sep}dataset"

  val LocalArtifactRepo = s"$StorageLocation${sep}repo"
}
