package backend

import java.nio.ByteBuffer

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.model.HttpEntity
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
import backend.requests.Sparql
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import frontend.webui.protocol.RequestSucceeded
import frontend.webui.protocol.RequestFailed

final class WebService(implicit system: ActorSystem)
    extends Directives
    with Sparql {

  override val bs = new BackendSystem()
  val log = system.log

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
      handleWebSocketMessages(authNvimUi())
    } ~
    path("auth-web") {
      handleWebSocketMessages(authWebUi())
    } ~
    path("communication") {
      parameter('name) { name ⇒
        handleWebSocketMessages(communicationFlow(sender = name))
      }
    } ~
    pathPrefix("kbws") {
      parameter('id) { id ⇒
        handleWebSocketMessages(withWebsocketFlow(bs.webUiFlow(id)))
      }
    } ~
    rejectEmptyResponse {
      path("favicon.ico")(getFromResource("favicon.ico", MediaTypes.`image/x-icon`))
    } ~
    pathPrefix("kb") {
      val content = Content.kbPage(
        cssDeps = Seq(
          "http://www.alpacajs.org/lib/bootstrap/dist/css/bootstrap.min.css",
          "http://www.alpacajs.org/lib/alpaca/bootstrap/alpaca.min.css"
        ),
        jsDeps = Seq(
          "web-ui-jsdeps.js", "web-ui-fastopt.js", "web-ui-launcher.js",
          "http://www.alpacajs.org/lib/handlebars/handlebars.min.js",
          "http://www.alpacajs.org/lib/bootstrap/dist/js/bootstrap.min.js",
          "http://www.alpacajs.org/lib/alpaca/bootstrap/alpaca.min.js"
        )
      )
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, content))
    } ~
    path("web-ui-jsdeps.js")(getFromResource("web-ui-jsdeps.js")) ~
    path("web-ui-fastopt.js")(getFromResource("web-ui-fastopt.js")) ~
    path("web-ui-launcher.js")(getFromResource("web-ui-launcher.js")) ~
    path("sparql") {
      parameterMap { params ⇒
        handleSparqlGetRequest(params)
      }
    } ~
    path("add-json") {
      val content = Content.addJsonPage(
        cssDeps = Seq(
          "http://www.alpacajs.org/lib/bootstrap/dist/css/bootstrap.min.css",
          "http://www.alpacajs.org/lib/alpaca/bootstrap/alpaca.min.css"
        ),
        jsDeps = Seq(
          "http://www.alpacajs.org/lib/jquery/dist/jquery.min.js",
          "http://www.alpacajs.org/lib/handlebars/handlebars.min.js",
          "http://www.alpacajs.org/lib/bootstrap/dist/js/bootstrap.min.js",
          "http://www.alpacajs.org/lib/alpaca/bootstrap/alpaca.min.js"
        )
      )
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, content))
    } ~
    path("queue") {
      parameter('item) { rawId ⇒
        val id = rawId.toInt
        onComplete(bs.queueItem(id)) {
          case scala.util.Success(logger) ⇒
            val content = Content.itemPage(id, logger)
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, content))

          case scala.util.Failure(f) ⇒
            import StatusCodes._
            complete(HttpResponse(InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
        }
      }
    } ~
    path("queue") {
      onComplete(bs.queueItems) {
        case scala.util.Success(items) ⇒
          val content = Content.queuePage(items)
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, content))

        case scala.util.Failure(f) ⇒
          import StatusCodes._
          complete(HttpResponse(InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
      }
    }
  } ~
  post {
    path("sparql") {
      entity(as[String]) { encodedPostReq ⇒
        extractRequest { req ⇒
          handleSparqlPostRequest(req, encodedPostReq)
        }
      }
    } ~
    path("add-json") {
      entity(as[String]) { json ⇒
        import scala.util._
        onComplete(bs.indexData(json)) {
          case Success(RequestSucceeded(msg)) ⇒
            complete(msg)
          case Success(RequestFailed(msg)) ⇒
            complete(msg)
          case Success(msg) ⇒
            import StatusCodes._
            log.error(s"Unexpected response for add-json request: $msg")
            complete(HttpResponse(InternalServerError, entity = s"Internal server error: No valid response provided."))
          case Failure(f) ⇒
            import StatusCodes._
            log.error(f, "Error happened while handling add-json request.")
            complete(HttpResponse(InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
        }
      }
    }
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

  private def authNvimUi(): Flow[Message, Message, NotUsed] =
    withWebsocketFlow(bs.authNvimUi())

  private def authWebUi(): Flow[Message, Message, NotUsed] =
    withWebsocketFlow(bs.authWebUi())

  private def communicationFlow(sender: String): Flow[Message, Message, NotUsed] =
    withWebsocketFlow(bs.nvimFlow(sender))

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
