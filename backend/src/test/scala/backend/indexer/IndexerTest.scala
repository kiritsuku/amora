package backend.indexer

import org.junit.Test

import akka.http.javadsl.model.headers.RawRequestURI
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.RouteTest
import akka.http.scaladsl.testkit.RouteTestTimeout
import akka.http.scaladsl.testkit.TestFrameworkInterface
import backend.AkkaLogging
import backend.CustomContentTypes
import backend.Log4jRootLogging
import backend.PlatformConstants
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

  implicit val timeout = {
    import scala.concurrent.duration._
    // wait for the time that is available to the server + some more
    RouteTestTimeout(PlatformConstants.timeout.duration + 500.millis)
  }

  val service = new WebService
  val route = service.route

  @Test
  def jsonld_context_can_be_retrieved(): Unit = {
    get("http://amora.center/kb/amora/Format/0.1/amora/Format/0.1/schema.jsonld?format=jsonld") ~> route ~> check {
      status === StatusCodes.OK
    }
  }

  @Test
  def error_for_invalid_format(): Unit = {
    get("http://amora.center/kb/amora/Format/0.1/amora/Format/0.1/schema.jsonld?format=invalid") ~> route ~> check {
      status === StatusCodes.BadRequest
    }
  }

  @Test
  def error_for_invalid_format_uri(): Unit = {
    get("http://amora.center/kb/amora/Format/0.1/amora/Format/0.1/invalid.jsonld?format=jsonld") ~> route ~> check {
      status === StatusCodes.NotFound
    }
  }

  @Test
  def sparql_post_requests_are_possible(): Unit = {
    post("http://amora.center/sparql", "query=select * where {?s ?p ?o} limit 3") ~> route ~> check {
      status === StatusCodes.OK
    }
  }

  private def post(uri: String, request: String) = {
    val u = Uri(uri)
    val r = HttpRequest(HttpMethods.POST, u, List(RawRequestURI.create(u.toRelative.toString), Accept(CustomContentTypes.`sparql-results+json`)), request)
    log.info(s"sending request: $r")
    r
  }

  private def get(uri: String) = {
    val u = Uri(uri)
    val r = HttpRequest(HttpMethods.GET, u, List(RawRequestURI.create(u.toRelative.toString)))
    log.info(s"sending request: $r")
    r
  }
}
