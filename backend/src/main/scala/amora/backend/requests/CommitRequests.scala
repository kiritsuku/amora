package amora.backend.requests

import scala.util.Failure
import scala.util.Success

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Directives
import amora.api.SparqlModel
import amora.api.Turtle
import amora.backend.AkkaLogging
import amora.backend.BackendSystem

trait CommitRequests extends Directives with AkkaLogging {
  import akka.http.scaladsl.model.ContentTypes._
  import amora.backend.CustomContentTypes._

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

  def handleShowCommitGetRequest(commit: String) = {
    bs.showCommit(commit, "Error while showing commit.") {
      case Success(m: SparqlModel) ⇒ HttpEntity(`text/turtle(UTF-8)`, m.formatAs(Turtle))
      case Failure(t) ⇒ throw t
    }
  }
}
