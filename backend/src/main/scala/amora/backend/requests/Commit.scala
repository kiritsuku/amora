package amora.backend.requests

import scala.util.Failure
import scala.util.Success

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Directives
import amora.backend.AkkaLogging
import amora.backend.BackendSystem

trait Commit extends Directives with AkkaLogging {
  import akka.http.scaladsl.model.ContentTypes._

  def bs: BackendSystem

  def handleHeadCommitGetRequest() = {
    bs.headCommit("Error while retrieving head commit.") {
      case Success(hash: String) ⇒ HttpEntity(`text/plain(UTF-8)`, hash)
      case Failure(t) ⇒ throw t
    }
  }

  def handleListCommitsGetRequest() = {
    bs.listCommits("Error while listing commits.") {
      case Success(hashes: List[_]) ⇒ HttpEntity(`text/plain(UTF-8)`, hashes.mkString(","))
      case Failure(t) ⇒ throw t
    }
  }
}
