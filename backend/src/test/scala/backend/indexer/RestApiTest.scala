package backend.indexer

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import scala.concurrent.Await
import scala.concurrent.Promise
import scala.concurrent.duration.Duration
import scala.util.Failure
import scala.util.Success

import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QuerySolution
import org.apache.jena.query.ResultSet
import org.apache.jena.query.ResultSetFactory
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.ModelFactory
import org.junit.After

import akka.actor.Cancellable
import akka.http.javadsl.model.headers.RawRequestURI
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.RequestEntity
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.RouteTest
import akka.http.scaladsl.testkit.RouteTestTimeout
import akka.http.scaladsl.testkit.TestFrameworkInterface

import backend.AkkaLogging
import backend.CustomContentTypes
import backend.IgnoreLogger
import backend.Log4jRootLogging
import backend.PlatformConstants
import backend.WebService
import backend.schema._
import research.converter.protocol._

trait RestApiTest extends TestFrameworkInterface with RouteTest with AkkaLogging with Log4jRootLogging {
  import backend.TestUtils._

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
  val config = system.settings.config
  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")
  val binding = Http().bindAndHandle(route, interface, port)
  binding.onComplete {
    case Success(binding) ⇒
      val addr = binding.localAddress
      log.info(s"Server is listening on ${addr.getHostName}:${addr.getPort}")
    case Failure(e) ⇒
      log.error(e, "Failed to start server")
      system.terminate()
  }

  case class Data(varName: String, value: String)

  def resultSetAsData(r: ResultSet): Seq[Seq[Data]] = {
    transformResultSet(r) { (v, q) ⇒
      val res = q.get(v)
      require(res != null, s"The variable `$v` does not exist in the result set.")
      val value =
        if (res.isLiteral())
          res.asLiteral().getString
        else
          res.toString()
      Data(v, value)
    }
  }

  def transformResultSet[A](r: ResultSet)(f: (String, QuerySolution) ⇒ A): Seq[Seq[A]] = {
    import scala.collection.JavaConverters._
    val vars = r.getResultVars.asScala.toSeq

    for (q ← r.asScala.toSeq) yield
      for (v ← vars) yield
        f(v, q)
  }

  def scheduleReq(req: ⇒ HttpRequest)(f: ⇒ Boolean): Unit = {
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

  def testReq[A](req: ⇒ HttpRequest)(f: ⇒ A): A = {
    val r = req
    r ~> route ~> check {
      val isJsonResponse = r.header[Accept].flatMap(_.mediaRanges.headOption).exists {
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

  def resultSetAsString(r: ResultSet): String = {
    val s = new ByteArrayOutputStream
    ResultSetFormatter.out(s, r)
    new String(s.toByteArray(), StandardCharsets.UTF_8)
  }

  def respAsString: String =
    Await.result(response.entity.dataBytes.runFold("")(_ + _.utf8String), timeout.duration)

  def respAsResultSet(): ResultSet = {
    val in = new ByteArrayInputStream(respAsString.getBytes(StandardCharsets.UTF_8))
    ResultSetFactory.makeRewindable(ResultSetFactory.fromJSON(in))
  }

  def post(uri: String, request: RequestEntity, header: HttpHeader*): HttpRequest = {
    val u = Uri(uri)
    val r = HttpRequest(HttpMethods.POST, u, List(RawRequestURI.create(u.toRelative.toString)) ++ header, request)
    log.info(s"sending request: $r")
    r
  }

  def get(uri: String): HttpRequest = {
    val u = Uri(uri)
    val r = HttpRequest(HttpMethods.GET, u, List(RawRequestURI.create(u.toRelative.toString)))
    log.info(s"sending request: $r")
    r
  }

  def showAmoraIndexContent(entries: Int = 100, prefix: String = "http://amora.center/kb/"): Unit = {
    require(entries > 0, "limit needs to be greater than zero")
    testReq(post("http://amora.center/sparql", s"""
      select * where {
        ?s ?p ?o .
        filter regex(str(?s), "^$prefix")
      }
      order by ?s ?p
      limit $entries
    """, header = Accept(CustomContentTypes.`sparql-results+json`))) {
      status === StatusCodes.OK
    }
  }

  def indexData(origin: Schema, data: (String, String)*): Unit = {
    val indexer = new ScalaSourceIndexer(IgnoreLogger)
    indexer.convertToHierarchy(data) match {
      case scala.util.Success(data) ⇒
        data foreach {
          case (filename, data) ⇒
            def asSchemaPackage(decl: Hierarchy): Schema = decl match {
              case Root ⇒ origin
              case Decl(name, owner) ⇒ Package(name, asSchemaPackage(owner))
              case _ ⇒ ???
            }
            val pkg = data.collectFirst {
              case d if d.attachments(Attachment.Package) ⇒ d
            }
            val s = pkg.map(asSchemaPackage).map(pkg ⇒ File(pkg, filename)).getOrElse(File(origin, filename))
            testReq(post("http://amora.center/sparql-update", Schema.mkSparqlUpdate(Seq(s)))) {(
              status === StatusCodes.OK)
            }
            val query = HierarchySchema.mkSparqlUpdate(s, data)
            testReq(post("http://amora.center/sparql-update", query)) {
              status === StatusCodes.OK
            }
        }
      case scala.util.Failure(f) ⇒
        throw f
    }
  }

  sealed trait Region extends Product with Serializable {
    def len: Int
  }
  case class Range(start: Int, end: Int, str: String) extends Region {
    override def len = "[[]]".length
  }
  case class Offset(offset: Int, str: String) extends Region {
    override def len = "[[!]]".length + str.length
  }

  /**
   * Runs a test against the indexer.
   *
   * `rawData` are tuples of the form `(filename, source)`. The sources will be
   * typechecked and indexed and once this is done the query will run against
   * the index.
   *
   * The sources can contain markers that start with `[[` and end with `]]`.
   * These markers are the start and end position of a range, which shall be
   * returned by the query. If the first character after the `[[` marker is a
   * exclamation mark, the range will become an offset region, whose start and
   * end position are the same. Offset regions need to be used when implicit
   * regions need to be tested for their existence (for example implicit return
   * types of methods). The sources are freed of the region markers before they
   * are passed to the typechecker to make it convenient to write tests.
   */
  def indexRegionData(query: String, origin: Schema, rawData: (String, String)*): Unit = {
    def findRegions(src: String, prevStart: Int, prevEnd: Int, regions: IndexedSeq[Region]): IndexedSeq[Region] = {
      val start = src.indexOf("[[", prevEnd)
      if (start < 0)
        regions
      else {
        val end = src.indexOf("]]", start)
        val len = regions.map(_.len).sum
        val isOffset = src(start + 2) == '!'
        val range =
          if (isOffset)
            Offset(start - len, src.substring(start + 3, end))
          else
            Range(start - len, end - len - "[[".length, src.substring(start + 2, end))
        findRegions(src, start, end, regions :+ range)
      }
    }
    val dataWithRegions = rawData map {
      case (filename, rawSrc) ⇒
        val regions = findRegions(rawSrc, 0, 0, Vector())
        val src = rawSrc.replaceAll("""\[\[!.*?\]\]|\[\[|\]\]""", "")
        (filename, src, regions)
    }
    val data = dataWithRegions.map { case (filename, src, _) ⇒ (filename, src) }
    indexData(origin, data: _*)

    val regionOrdering: Region ⇒ (Int, Int, String) = {
      case Range(start, end, text) ⇒ (start, end, text)
      case Offset(offset, text) ⇒ (offset, offset, text)
    }
    val expectedRegions = dataWithRegions.flatMap { case (_, _, region) ⇒ region }.sortBy(regionOrdering)

    testReq(post("http://amora.center/sparql", query, header = Accept(CustomContentTypes.`sparql-results+json`))) {
      status === StatusCodes.OK
      val r = respAsResultSet()

      import scala.collection.JavaConverters._
      val foundRegions = r.asScala.toSeq.map { row ⇒
        val start = row.get("start")
        require(start != null, "No field with name `start` found.")
        val end = row.get("end")
        require(end != null, "No field with name `end` found.")
        val name = row.get("name")
        require(name != null, "No field with name `name` found.")

        if (start.asLiteral().getInt == end.asLiteral().getInt)
          Offset(start.asLiteral().getInt, name.toString())
        else
          Range(start.asLiteral().getInt, end.asLiteral().getInt, name.toString)
      }.sortBy(regionOrdering)

      foundRegions === expectedRegions
    }
  }

  def sparqlRequest(query: String): Seq[Seq[Data]] = {
    testReq(post("http://amora.center/sparql", query, header = Accept(CustomContentTypes.`sparql-results+json`))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet())
    }
  }

  def serviceRequest(query: String): Model = {
    testReq(post("http://amora.center/service", HttpEntity(CustomContentTypes.`text/n3(UTF-8)`, query), header = Accept(CustomContentTypes.n3))) {
      status === StatusCodes.OK
      fillModel(ModelFactory.createDefaultModel(), respAsString)
    }
  }

  def modelAsData(model: Model, query: String): Seq[Seq[Data]] = {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), model)
    val rs = ResultSetFactory.makeRewindable(qexec.execSelect())
    if (debugTests) {
      log.info("response as query result:\n" + resultSetAsString(rs))
      rs.reset()
    }
    resultSetAsData(rs)
  }

  def fillModel(m: Model, n3Data: String): Model = {
    val in = new ByteArrayInputStream(n3Data.getBytes)
    m.read(in, null, "N3")
    m
  }

  case class CursorData(cursorPos: Int, src: String)

  /**
   * Takes a string as input that contains the '^' character, which donates the
   * position of a cursor. The index of the cursor is returned together with the
   * string that no longer contains the cursor.
   */
  def cursorData(rawSrc: String): CursorData = {
    val i = rawSrc.indexOf('^')
    require(i >= 0, "No cursor marker found.")
    CursorData(i, rawSrc.substring(0, i) + rawSrc.substring(i+1))
  }

  @After
  def waitForTermination(): Unit = {
    Await.result(system.terminate(), Duration.Inf)
  }
}
