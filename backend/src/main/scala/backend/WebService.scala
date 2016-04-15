package backend

import java.net.URLDecoder

import java.nio.ByteBuffer

import org.apache.jena.sparql.resultset.ResultsFormat

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.model.HttpCharsets
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaType
import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.model.ws.BinaryMessage
import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.server.Directives
import akka.stream.Attributes
import akka.stream.FlowShape
import akka.stream.Inlet
import akka.stream.Materializer
import akka.stream.Outlet
import akka.stream.scaladsl.Flow
import akka.stream.stage.GraphStage
import akka.stream.stage.GraphStageLogic
import akka.stream.stage.InHandler
import akka.stream.stage.OutHandler
import akka.util.CompactByteString
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.UnacceptedResponseContentTypeRejection
import akka.http.scaladsl.server.ContentNegotiator

final class WebService(implicit m: Materializer, system: ActorSystem) extends Directives {

  private val bs = new BackendSystem()

  def route = get {
    pathSingleSlash(complete {
      val content = Content.indexPage(
        cssDeps = Seq("default.css", "codemirror.css", "solarized.css"),
        jsDeps = Seq("clike.js", "markdown.js", "ui-fastopt.js", "ui-launcher.js")
      )
      HttpEntity(ContentTypes.`text/html(UTF-8)`, content)
    }) ~
    path("ui-jsdeps.js")(getFromResource("ui-jsdeps.js")) ~
    path("ui-fastopt.js")(getFromResource("ui-fastopt.js")) ~
    path("ui-launcher.js")(getFromResource("ui-launcher.js")) ~
    path("marked.js")(getFromResource("marked/lib/marked.js")) ~
    path("clike.js")(getFromResource("codemirror/mode/clike/clike.js")) ~
    path("markdown.js")(getFromResource("codemirror/mode/markdown/markdown.js")) ~
    path("default.css")(getFromResource("default.css")) ~
    path("codemirror.css")(getFromResource("codemirror/lib/codemirror.css")) ~
    path("solarized.css")(getFromResource("codemirror/theme/solarized.css")) ~
    path("auth") {
      handleWebSocketMessages(authClientFlow())
    } ~
    path("communication") {
      parameter('name) { name ⇒
        handleWebSocketMessages(communicationFlow(sender = name))
      }
    } ~
    rejectEmptyResponse {
      path("favicon.ico")(getFromResource("favicon.ico", MediaTypes.`image/x-icon`))
    } ~
    pathPrefix("kb") {
      path(RestPath) { path ⇒
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"got: $path"))
      }
    } ~
    path("sparql") {
      parameterMap { params ⇒
        val ret =
          if (params.isEmpty)
            showSparqlEditor()
          else if (params.contains("query"))
            HttpEntity(CustomContentTypes.`sparql-results+json(UTF-8)`, askQuery(params("query"), ResultsFormat.FMT_RS_JSON))
          else
            ???

        complete(ret)
      }
    }
  } ~
  post {
    path("sparql") {
      entity(as[String]) { str ⇒
        extractRequest { req ⇒
          import CustomContentTypes._
          val ct = req.header[Accept].flatMap(_.mediaRanges.headOption).collect {
            case m if m matches `sparql-results+xml`  ⇒ `sparql-results+xml(UTF-8)` → ResultsFormat.FMT_RS_XML
            case m if m matches `sparql-results+json` ⇒ `sparql-results+json(UTF-8)` → ResultsFormat.FMT_RS_JSON
          }
          val resp = ct.map {
            case (ct, fmt) ⇒
              val decoded = URLDecoder.decode(str, "UTF-8")
              println("(post) str: " + decoded)
              complete(HttpEntity(ct, askQuery("select * where {?s ?p ?o} limit 1", fmt)))
          }
          resp.getOrElse {
            reject(UnacceptedResponseContentTypeRejection(Set(
                ContentNegotiator.Alternative(`sparql-results+json`),
                ContentNegotiator.Alternative(`sparql-results+xml`)
            )))
          }
        }
      }
    } ~
    path("add") {
      entity(as[String]) { str ⇒
        testAddData(str)
        complete(s"data added")
      }
    }
  }

  private def askQuery(query: String, fmt: ResultsFormat) = {
    bs.askQuery(query, fmt) match {
      case scala.util.Success(s) ⇒ s
      case scala.util.Failure(f) ⇒ f.printStackTrace(); "{}"
    }
  }

  object CustomContentTypes {
    val `sparql-results+xml` = MediaType.applicationWithOpenCharset("sparql-results+xml")
    val `sparql-results+json` = MediaType.applicationWithOpenCharset("sparql-results+json")

    val `sparql-results+xml(UTF-8)` = `sparql-results+xml` withCharset HttpCharsets.`UTF-8`
    val `sparql-results+json(UTF-8)` = `sparql-results+json` withCharset HttpCharsets.`UTF-8`
  }

  private def showSparqlEditor() = {
    val content = Content.sparql(
      cssDeps = Seq("http://cdn.jsdelivr.net/yasgui/2.2.1/yasgui.min.css"),
      jsDeps = Seq("http://cdn.jsdelivr.net/yasgui/2.2.1/yasgui.min.js")
    )
    HttpEntity(ContentTypes.`text/html(UTF-8)`, content)
  }

  private def testAddData(str: String): Unit = {
    import research.indexer.hierarchy._
    bs.addData("test.scala", Seq(Decl(str, Root)))
  }

  private def withWebsocketFlow(flow: Flow[ByteBuffer, ByteBuffer, NotUsed]): Flow[Message, Message, NotUsed] =
    Flow[Message]
    .collect {
      case BinaryMessage.Strict(bs) ⇒ bs.toByteBuffer
    }
    .via(flow)
    .map {
      case c ⇒ BinaryMessage(CompactByteString(c))
    }
    .via(reportErrorsFlow())

  private def authClientFlow(): Flow[Message, Message, NotUsed] =
    withWebsocketFlow(bs.authFlow())

  private def communicationFlow(sender: String): Flow[Message, Message, NotUsed] =
    withWebsocketFlow(bs.messageFlow(sender))

  private def reportErrorsFlow[A](): Flow[A, A, NotUsed] =
    Flow[A].via(new GraphStage[FlowShape[A, A]] {
      val in = Inlet[A]("in")
      val out = Outlet[A]("out")
      override val shape = FlowShape(in, out)
      override def createLogic(atts: Attributes) = new GraphStageLogic(shape) {
        setHandler(in, new InHandler {
          override def onPush() =
            push(out, grab(in))
          override def onUpstreamFailure(cause: Throwable) =
            system.log.error(cause, "WebService stream failed")
        })
        setHandler(out, new OutHandler {
          override def onPull() =
            pull(in)
        })
      }
    })
}
