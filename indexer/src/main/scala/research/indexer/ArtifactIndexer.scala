package research.indexer

import java.io.ByteArrayOutputStream

import java.io.File
import java.io.InputStream
import java.util.zip.ZipEntry
import java.util.zip.ZipFile

import scala.util.Try

import research.converter.ClassfileConverter
import research.converter.protocol.Hierarchy
import scalaz.{ Success ⇒ _, _ }
import scalaz.concurrent.Task
import research.Logger

object ArtifactIndexer {
  sealed trait DownloadStatus {
    def isError: Boolean
  }
  case class DownloadSuccess(artifact: File) extends DownloadStatus {
    override def isError = false
  }
  case class DownloadError(artifactName: String, reason: Option[String]) extends DownloadStatus {
    override def isError = true
  }
}

final class ArtifactIndexer(val logger: Logger) {
  import ArtifactIndexer._
  import coursier._

  def fetchArtifact(organization: String, name: String, version: String): Seq[DownloadStatus] = {
    val start = Resolution(Set(Dependency(Module(organization, name), version)))
    val repos = Seq(MavenRepository("https://repo1.maven.org/maven2"))
    val cache = new File(IndexerConstants.LocalArtifactRepo)
    val fetch = Fetch.from(repos, Cache.fetch(cache = cache, logger = Some(coursierLogger)))
    val resolution = start.process.run(fetch).run

    if (resolution.errors.nonEmpty)
      resolution.errors.flatMap(_._2).map(DownloadError(_, None))
    else {
      val localArtifacts = Task.gatherUnordered(
        resolution.artifacts.map(Cache.file(_, cache = cache, logger = Some(coursierLogger)).run)
      ).run

      localArtifacts.map {
        case -\/(f) ⇒
          DownloadError(f.message, Some(f.`type`))
        case \/-(file) ⇒
          DownloadSuccess(file)
      }
    }
  }

  def indexArtifact(artifact: File): Try[Seq[(String, Seq[Hierarchy])]] = Try {
    import scala.collection.JavaConverters._
    require(artifact.getName.endsWith(".jar"), "Artifact needs to be a JAR file")

    def entryToHierarchy(zip: ZipFile, entry: ZipEntry) = {
      val bytes = using(zip.getInputStream(entry))(readInputStream)
      val hierarchy = new ClassfileConverter().convert(bytes).get
      entry.getName → hierarchy
    }

    def zipToHierarchy(zip: ZipFile) = {
      val entries = zip.entries().asScala
      val filtered = entries.filter(_.getName.endsWith(".class"))
      filtered.map(entry ⇒ entryToHierarchy(zip, entry)).toList
    }

    using(new ZipFile(artifact))(zipToHierarchy)
  }

  private def readInputStream(in: InputStream): Array[Byte] = {
    val out = new ByteArrayOutputStream

    var nRead = 0
    val bytes = new Array[Byte](1 << 12)
    while ({nRead = in.read(bytes, 0, bytes.length); nRead != -1})
      out.write(bytes, 0, nRead)

    out.toByteArray()
  }

  private def using[A <: { def close(): Unit }, B](closeable: A)(f: A ⇒ B): B =
    try f(closeable) finally closeable.close()

  private val coursierLogger = new Cache.Logger {
    override def foundLocally(url: String, file: File): Unit = {
      logger.info(s"[found-locally] url: $url, file: $file")
    }

    override def downloadingArtifact(url: String, file: File): Unit = {
      logger.info(s"[downloading-artifact] url: $url, file: $file")
    }

    override def downloadLength(url: String, totalLength: Long, alreadyDownloaded: Long): Unit = {
      logger.info(s"[download-length] url: $url, total-length: $totalLength, already-downloaded: $alreadyDownloaded")
    }

    override def downloadProgress(url: String, downloaded: Long): Unit = {
      logger.info(s"[download-progress] url: $url, downloaded: $downloaded")
    }

    override def downloadedArtifact(url: String, success: Boolean): Unit = {
      logger.info(s"[downloaded-artifact] url: $url, success: $success")
    }

    override def checkingUpdates(url: String, currentTimeOpt: Option[Long]): Unit = {
      logger.info(s"[checking-updates] url: $url, current-time-opt: $currentTimeOpt")
    }

    override def checkingUpdatesResult(url: String, currentTimeOpt: Option[Long], remoteTimeOpt: Option[Long]): Unit = {
      logger.info(s"[checking-updates-result] url: $url, current-time-opt: $currentTimeOpt, remote-time-opt: $remoteTimeOpt")
    }
  }
}
