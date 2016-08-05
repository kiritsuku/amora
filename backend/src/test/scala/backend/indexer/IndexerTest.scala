package backend.indexer

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

import scala.concurrent.Await
import scala.concurrent.Promise

import org.apache.jena.query.QuerySolution
import org.apache.jena.query.ResultSet
import org.apache.jena.query.ResultSetFactory
import org.apache.jena.query.ResultSetFormatter
import org.junit.Test

import akka.actor.Cancellable
import akka.http.javadsl.model.headers.RawRequestURI
import akka.http.scaladsl.model.HttpHeader
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
import backend.schema.Project
import backend.schema.Schema
import backend.schema.Artifact

class IndexerTest extends TestFrameworkInterface with RouteTest with AkkaLogging with Log4jRootLogging {
  import TestUtils._

  override def failTest(msg: String): Nothing = {
    throw new RuntimeException(msg)
  }

  override def testConfigSource = s"""
    akka {
      loglevel = INFO

      # We need to access the raw URIs because they are used as keys in the index.
      http.server.raw-request-uri-header = on
    }
    app {
      interface = "localhost"
      port = 7777
      test-mode = true
      forward-internal-logger-to-akka-logger = true

      storage {
        location = "$binDir/amora"
        index-dataset = "$binDir/amora/dataset"
        artifact-repo = "$binDir/amora/repo"
      }
    }
  """

  private def binDir =
    getClass.getClassLoader.getResource(".").getPath

  implicit val timeout = {
    import scala.concurrent.duration._
    // wait for the time that is available to the server + some more
    RouteTestTimeout(PlatformConstants.timeout.duration + 500.millis)
  }

  val service = new WebService
  val route = service.route

  @Test
  def jsonld_context_can_be_retrieved(): Unit = {
    testReq(get("http://amora.center/kb/amora/Format/0.1/amora/Format/0.1/schema.jsonld?format=jsonld")) {
      status === StatusCodes.OK
    }
  }

  @Test
  def error_for_invalid_format(): Unit = {
    testReq(get("http://amora.center/kb/amora/Format/0.1/amora/Format/0.1/schema.jsonld?format=invalid")) {
      status === StatusCodes.BadRequest
    }
  }

  @Test
  def error_for_invalid_format_uri(): Unit = {
    testReq(get("http://amora.center/kb/amora/Format/0.1/amora/Format/0.1/invalid.jsonld?format=jsonld")) {
      status === StatusCodes.NotFound
    }
  }

  @Test
  def sparql_post_requests_are_possible(): Unit = {
    testReq(post("http://amora.center/sparql", "query=select * where {?s ?p ?o} limit 3", header = Accept(CustomContentTypes.`sparql-results+json`))) {
      status === StatusCodes.OK
    }
  }

  @Test
  def add_json_post_requests_are_possible(): Unit = {
    testReq(post("http://amora.center/add-json", """
      {
        "tpe":"artifact",
        "artifacts":[
          {
            "organization":"com.freedomotic",
            "name":"hello-world",
            "version":"3.0"
          }
        ]
      }
    """)) {
      status === StatusCodes.OK
    }
    scheduleReq(post("http://amora.center/itemFinished?id=1", "")) {
      status === StatusCodes.OK
      respAsString.toBoolean
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix kb:<http://amora.center/kb/>
      select * where {
        [a kb:Project] kb:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      val r = respAsResultSet()
      status === StatusCodes.OK
      r.next().get("name").asLiteral().getString === "hello-world"
    }
  }

  @Test
  def add_single_project(): Unit = {
    val q = Schema.mkSparqlUpdate(Seq(Project("p")))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      select * where {
        [a p:] p:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(Seq(Data("name", "p")))
    }
  }

  @Test
  def add_multiple_projects(): Unit = {
    val q = Schema.mkSparqlUpdate(Seq(Project("p1"), Project("p2")))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      select ?name where {
        [a p:] p:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "p1")),
          Seq(Data("name", "p2")))
    }
  }

  @Test
  def add_single_artifact(): Unit = {
    val a = Artifact(Project("p"), "o", "n", "v1")
    val q = Schema.mkSparqlUpdate(Seq(a))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      select ?organization ?name ?version where {
        [a a:] a:organization ?organization ; a:name ?name ; a:version ?version .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("organization", "o"), Data("name", "n"), Data("version", "v1")))
    }
  }

  @Test
  def add_multiple_artifacts(): Unit = {
    val p = Project("p")
    val a1 = Artifact(p, "o1", "n1", "v1")
    val a2 = Artifact(p, "o2", "n2", "v2")
    val q = Schema.mkSparqlUpdate(Seq(a1, a2))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      select ?organization ?name ?version where {
        [a a:] a:organization ?organization ; a:name ?name ; a:version ?version .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("organization", "o1"), Data("name", "n1"), Data("version", "v1")),
          Seq(Data("organization", "o2"), Data("name", "n2"), Data("version", "v2")))
    }
  }

  private case class Data(varName: String, value: String)

  private def resultSetAsData(r: ResultSet): Seq[Seq[Data]] = {
    transformResultSet(r) { (v, q) ⇒
      val res = q.get(v)
      require(res != null, s"The variable `$v` does not exist in the result set.")
      Data(v, res.asLiteral().getString)
    }
  }

  private def transformResultSet[A](r: ResultSet)(f: (String, QuerySolution) ⇒ A): Seq[Seq[A]] = {
    import scala.collection.JavaConverters._
    val vars = r.getResultVars.asScala.toSeq

    for (q ← r.asScala.toSeq) yield
      for (v ← vars) yield
        f(v, q)
  }

  private def scheduleReq(req: ⇒ HttpRequest)(f: ⇒ Boolean) = {
    import scala.concurrent.duration._
    val p = Promise[Unit]
    var cancellable: Cancellable = null
    try {
      cancellable = system.scheduler.schedule(100.millis, 100.millis) {
        req ~> route ~> check {
          val res = f
          if (res)
            p.success(())
        }
      }
      Await.ready(p.future, Duration.Inf)
    } finally {
      cancellable.cancel()
    }
  }

  private def testReq(req: ⇒ HttpRequest)(f: ⇒ Unit) = {
    req ~> route ~> check {
      val isJsonResponse = req.header[Accept].flatMap(_.mediaRanges.headOption).exists {
        case m if m matches CustomContentTypes.`sparql-results+json` ⇒ true
        case _ ⇒ false
      }
      if (debugTests && status == StatusCodes.OK && isJsonResponse) {
        val r = respAsResultSet()
        log.info("response as query result:\n" + resultSetAsString(r))
      }
      f
    }
  }

  private def resultSetAsString(r: ResultSet): String = {
    val s = new ByteArrayOutputStream
    ResultSetFormatter.out(s, r)
    new String(s.toByteArray(), "UTF-8")
  }

  private def respAsString: String =
    Await.result(response.entity.dataBytes.runFold("")(_ + _.utf8String), timeout.duration)

  private def respAsResultSet(): ResultSet = {
    val in = new ByteArrayInputStream(respAsString.getBytes)
    ResultSetFactory.makeRewindable(ResultSetFactory.fromJSON(in))
  }

  private def post(uri: String, request: String, header: HttpHeader*) = {
    val u = Uri(uri)
    val r = HttpRequest(HttpMethods.POST, u, List(RawRequestURI.create(u.toRelative.toString)) ++ header, request)
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
