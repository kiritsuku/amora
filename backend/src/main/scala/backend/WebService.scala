package backend

import java.nio.ByteBuffer

import scala.util.Try

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.javadsl.model.headers.RawRequestURI
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.ws.BinaryMessage
import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Route
import akka.stream.Attributes
import akka.stream.FlowShape
import akka.stream.Inlet
import akka.stream.Outlet
import akka.stream.scaladsl.Flow
import akka.stream.stage.GraphStage
import akka.stream.stage.GraphStageLogic
import akka.stream.stage.InHandler
import akka.stream.stage.OutHandler
import akka.util.CompactByteString
import backend.requests.Sparql
import frontend.webui.protocol.RequestFailed
import frontend.webui.protocol.RequestSucceeded

final class WebService(override implicit val system: ActorSystem)
    extends Directives
    with Sparql
    with AkkaLogging {

  override val bs = new BackendSystem()

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
    path("kbws") {
      parameter('id) { id ⇒
        handleWebSocketMessages(withWebsocketFlow(bs.webUiFlow(id)))
      }
    } ~
    rejectEmptyResponse {
      path("favicon.ico")(getFromResource("favicon.ico", MediaTypes.`image/x-icon`))
    } ~
    path("kb") {
      val content = Content.kbPage(
        cssDeps = Seq(
          "http://www.alpacajs.org/lib/bootstrap/dist/css/bootstrap.min.css",
          "http://www.alpacajs.org/lib/alpaca/bootstrap/alpaca.min.css"
        ),
        jsDeps = Seq(
          "web-ui-jsdeps.js", "web-ui-fastopt.js", "web-ui-launcher.js",
          "http://www.alpacajs.org/lib/handlebars/handlebars.min.js",
          "http://www.alpacajs.org/lib/bootstrap/dist/js/bootstrap.min.js",
          "http://www.alpacajs.org/lib/alpaca/bootstrap/alpaca.min.js",
          "http://www.alpacajs.org/lib/ace-builds/src-min-noconflict/ace.js"
        )
      )
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, content))
    } ~
    pathPrefix("kb" ~ Slash) {
      extractRequest { req ⇒
        rawRequestUri(req) { (path, query) ⇒
          query.get("format") match {
            case Some("jsonld") ⇒
              retrieveJsonLdContext(path)
            case Some(format) ⇒
              complete(HttpResponse(StatusCodes.BadRequest, entity = s"Parameter `format` has invalid value `$format`."))
            case _ ⇒
              handleKbPathGetRequest(path)
          }
        }
      }
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
            complete(HttpResponse(StatusCodes.InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
        }
      }
    } ~
    path("queue") {
      onComplete(bs.queueItems) {
        case scala.util.Success(items) ⇒
          val content = Content.queuePage(items)
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, content))

        case scala.util.Failure(f) ⇒
          complete(HttpResponse(StatusCodes.InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
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
            log.error(s"Unexpected response for add-json request: $msg")
            complete(HttpResponse(StatusCodes.BadRequest, entity = s"No valid response provided."))
          case Failure(f) ⇒
            log.error(f, "Error happened while handling add-json request.")
            complete(HttpResponse(StatusCodes.InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
        }
      }
    } ~
    pathPrefix("kb" ~ Slash) {
      extractRequest { req ⇒
        rawRequestUri(req) { (path, query) ⇒
          handleKbPathPostRequest(path)
        }
      }
    } ~
    path("itemFinished") {
      extractRequest { req ⇒
        rawRequestUri(req) { (path, query) ⇒
          query.get("id") match {
            case Some(id) if Try(id.toInt).isSuccess ⇒
              onComplete(bs.queueItem(id.toInt)) {
                case scala.util.Success(item) ⇒
                  complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, item.isClosed.toString))
                case scala.util.Failure(f) ⇒
                  log.error(f, s"Error happened while handling `$path?$query` request.")
                  complete(HttpResponse(StatusCodes.InternalServerError, entity = s"Internal server error: ${f.getMessage}"))
              }
            case _ ⇒
              complete(HttpResponse(StatusCodes.BadRequest, entity = s"Parameter `id` does not exist or has an invalid value."))
          }
        }
      }
    } ~
    path("sparql-update") {
      entity(as[String]) { encodedPostReq ⇒
        extractRequest { req ⇒
          handleSparqlUpdatePostRequest(req, encodedPostReq)
        }
      }
    }
  }

  private def rawRequestUri(req: HttpRequest)(f: (String, Query) ⇒ Route): Route = {
    req.header[RawRequestURI] match {
      case Some(rawUri) ⇒
        val queryLen = req.uri.rawQueryString.map(_.length + "?".length).getOrElse(0)
        val uri = req.uri
        val path = s"${uri.scheme}:${uri.authority}${rawUri.uri.dropRight(queryLen)}"
        f(path, uri.query())
      case _ ⇒
        throw new InternalError("Header Raw-Request-URI not found. Enable them in the configuration file.")
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
