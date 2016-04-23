package research.indexer

import java.io.File

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import scalaz.{ Success ⇒ _, _ }
import scalaz.concurrent.Task

trait ArtifactIndexer {

  import coursier._

  def fetchArtifact(organization: String, name: String, version: String): Try[String] = {
    val start = Resolution(Set(Dependency(Module(organization, name), version)))
    val repos = Seq(MavenRepository("https://repo1.maven.org/maven2"))
    val cache = new File(IndexerConstants.LocalArtifactRepo)
    val fetch = Fetch.from(repos, Cache.fetch(cache = cache, logger = Some(logger)))
    val resolution = start.process.run(fetch).run

    if (resolution.errors.nonEmpty)
      Failure(new RuntimeException("Couldn't find dependencies:"+resolution.errors.flatMap(_._2).mkString("\n    ", "    \n", "")))
    else {
      val localArtifacts = Task.gatherUnordered(
        resolution.artifacts.map(Cache.file(_, cache = cache, logger = Some(logger)).run)
      ).run

      val fileNames = localArtifacts.map {
        case -\/(f) ⇒
          throw new RuntimeException(s"Error occurred while downloading file `${f.message}`: ${f.`type`}")
        case \/-(file) ⇒
          file.getName
      }
      Success("Successfully downloaded dependencies:"+fileNames.mkString("\n    ", "    \n", ""))
    }
  }

  private val logger = new Cache.Logger {
    override def foundLocally(url: String, file: File): Unit = {
      println(s"[found-locally] url: $url, file: $file")
    }

    override def downloadingArtifact(url: String, file: File): Unit = {
      println(s"[downloading-artifact] url: $url, file: $file")
    }

    override def downloadLength(url: String, totalLength: Long, alreadyDownloaded: Long): Unit = {
      println(s"[download-length] url: $url, total-length: $totalLength, already-downloaded: $alreadyDownloaded")
    }

    override def downloadProgress(url: String, downloaded: Long): Unit = {
      //      println(s"[download-progress] url: $url, downloaded: $downloaded")
    }

    override def downloadedArtifact(url: String, success: Boolean): Unit = {
      println(s"[downloaded-artifact] url: $url, success: $success")
    }

    override def checkingUpdates(url: String, currentTimeOpt: Option[Long]): Unit = {
      println(s"[checking-updates] url: $url, current-time-opt: $currentTimeOpt")
    }

    override def checkingUpdatesResult(url: String, currentTimeOpt: Option[Long], remoteTimeOpt: Option[Long]): Unit = {
      println(s"[checking-updates-result] url: $url, current-time-opt: $currentTimeOpt, remote-time-opt: $remoteTimeOpt")
    }
  }
}
