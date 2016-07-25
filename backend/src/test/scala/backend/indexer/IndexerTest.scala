package backend.indexer

import org.junit.Test

import akka.http.javadsl.model.headers.RawRequestURI
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.Uri

import akka.http.scaladsl.testkit.RouteTest
import akka.http.scaladsl.testkit.TestFrameworkInterface
import backend.AkkaLogging
import backend.Log4jRootLogging
import backend.TestUtils
import backend.WebService

class IndexerTest extends TestFrameworkInterface with RouteTest with AkkaLogging with Log4jRootLogging {
  import TestUtils._

  override def failTest(msg: String): Nothing = {
    throw new RuntimeException(msg)
  }

  override def testConfigSource = """
    akka {
      loglevel = INFO

      # We need to access the raw URIs because they are used as keys in the index.
      http.server.raw-request-uri-header = on
    }
    app {
      interface = "localhost"
      port = 7777
      test-mode = true
    }
  """

  @Test
  def jsonld_context_can_be_retrieved(): Unit = {
    val service = new WebService
    get("http://amora.center/kb/amora/Format/0.1/amora/Format/0.1/schema.jsonld?format=jsonld") ~> service.route ~> check {
      response.status === StatusCodes.OK
    }
  }

  private def get(uri: String) = {
    val u = Uri(uri)
    HttpRequest(HttpMethods.GET, u, List(RawRequestURI.create(u.toRelative.toString)))
  }
}
