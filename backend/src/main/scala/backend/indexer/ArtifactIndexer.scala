package backend.indexer

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
import backend.Logger
import akka.actor.Actor
import backend.actors.RequestMessage
import akka.actor.ActorRef
import backend.actors.IndexerMessage
import backend.actors.QueueMessage

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

final class ArtifactIndexer(indexer: ActorRef, logger: Logger) extends Actor {
  import ArtifactIndexer._
  import coursier._

  override def receive = {
    case RequestMessage.Artifacts(_, artifacts) ⇒
      handleArtifact(artifacts)
      sender ! QueueMessage.Completed
  }

  def handleArtifact(artifacts: Seq[RequestMessage.Artifact]) = {
    val res = artifacts.flatMap { artifact ⇒
      import artifact._
      fetchArtifact(organization, name, version)
    }
    val (errors, succs) = res.partition(_.isError)
    val succMsgs = succs.collect {
      case DownloadSuccess(artifact) ⇒
        artifact.getName
    }
    val errMsgs = errors.collect {
      case DownloadError(artifactName, reasonOpt) ⇒
        if (reasonOpt.isDefined)
          artifactName+"because of: "+reasonOpt.get
        else
          artifactName
    }
    val succMsg = if (succs.isEmpty) Nil else Seq(s"Fetched artifacts:" + succMsgs.sorted.mkString("\n  ", "\n  ", ""))
    val errMsg = if (errors.isEmpty) Nil else Seq(s"Failed to fetch artifacts:" + errMsgs.sorted.mkString("\n  ", "\n  ", ""))
    val msg = Seq(succMsg, errMsg).flatten.mkString("\n")

    logger.info(msg)

    if (errors.isEmpty)
      indexArtifacts(succs)
  }

  def indexArtifacts(artifacts: Seq[DownloadStatus]) = {
    logger.info(s"No errors happened during fetching of artifacts. Start indexing of ${artifacts.size} artifacts now.")
    val indexed = artifacts.collect {
      case DownloadSuccess(artifact) ⇒ indexArtifact(artifact).get
    }.flatten

    logger.info(s"Indexing ${indexed.size} files.")
    indexed.zipWithIndex foreach {
      case ((fileName, hierarchy), i) ⇒
        logger.info(s"Indexing file $i ($fileName) with ${hierarchy.size} entries.")
        this.indexer ! IndexerMessage.AddFile(fileName, hierarchy)
    }
    logger.info(s"Successfully indexed ${artifacts.size} artifacts.")
  }

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
