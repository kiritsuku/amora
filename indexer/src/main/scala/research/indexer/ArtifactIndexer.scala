package research.indexer

import java.io.File

import scalaz.{ Success ⇒ _, _ }
import scalaz.concurrent.Task

trait ArtifactIndexer {

  import coursier._

  sealed trait DownloadStatus
  case class DownloadSuccess(artifactName: String) extends DownloadStatus
  case class DownloadError(artifactName: String, reason: Option[String]) extends DownloadStatus

  def fetchArtifact(organization: String, name: String, version: String): Seq[DownloadStatus] = {
    val start = Resolution(Set(Dependency(Module(organization, name), version)))
    val repos = Seq(MavenRepository("https://repo1.maven.org/maven2"))
    val cache = new File(IndexerConstants.LocalArtifactRepo)
    val fetch = Fetch.from(repos, Cache.fetch(cache = cache, logger = Some(logger)))
    val resolution = start.process.run(fetch).run

    if (resolution.errors.nonEmpty)
      resolution.errors.flatMap(_._2).map(DownloadError(_, None))
    else {
      val localArtifacts = Task.gatherUnordered(
        resolution.artifacts.map(Cache.file(_, cache = cache, logger = Some(logger)).run)
      ).run

      localArtifacts.map {
        case -\/(f) ⇒
          DownloadError(f.message, Some(f.`type`))
        case \/-(file) ⇒
          DownloadSuccess(file.getName)
      }
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
