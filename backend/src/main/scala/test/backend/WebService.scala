package test.backend

import java.nio.ByteBuffer

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.model.ws.BinaryMessage
import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.server.Directives
import akka.stream.Materializer
import akka.stream.scaladsl.Flow
import akka.stream.stage.Context
import akka.stream.stage.PushStage
import akka.util.CompactByteString

final class WebService(implicit m: Materializer, system: ActorSystem) extends Directives {

  private val bs = new BackendSystem()

  def route = get {
    pathSingleSlash(complete {
      val content = Content.indexPage(
        cssDeps = Seq("default.css", "codemirror.css", "solarized.css"),
        jsDeps = Seq("scalajs-test-ui-fastopt.js", "clike.js", "markdown.js", "scalajs-test-ui-launcher.js")
      )
      HttpEntity(MediaTypes.`text/html`, content)
    }) ~
    path("scalajs-test-ui-jsdeps.js")(getFromResource("scalajs-test-ui-jsdeps.js")) ~
    path("scalajs-test-ui-fastopt.js")(getFromResource("scalajs-test-ui-fastopt.js")) ~
    path("scalajs-test-ui-launcher.js")(getFromResource("scalajs-test-ui-launcher.js")) ~
    path("marked.js")(getFromResource("marked/lib/marked.js")) ~
    path("clike.js")(getFromResource("codemirror/mode/clike/clike.js")) ~
    path("markdown.js")(getFromResource("codemirror/mode/markdown/markdown.js")) ~
    path("default.css")(getFromResource("default.css")) ~
    path("codemirror.css")(getFromResource("codemirror/lib/codemirror.css")) ~
    path("solarized.css")(getFromResource("codemirror/theme/solarized.css")) ~
    path("auth") {
      handleWebsocketMessages(authClientFlow())
    } ~
    path("communication") {
      parameter('name) { name ⇒
        handleWebsocketMessages(communicationFlow(sender = name))
      }
    } ~
    rejectEmptyResponse {
      path("favicon.ico")(getFromResource("favicon.ico", MediaTypes.`image/x-icon`))
    }
  }

  private def withWebsocketFlow(flow: Flow[ByteBuffer, ByteBuffer, Unit]): Flow[Message, Message, Unit] =
    Flow[Message]
    .collect {
      case BinaryMessage.Strict(bs) ⇒ bs.toByteBuffer
    }
    .via(flow)
    .map {
      case c ⇒ BinaryMessage(CompactByteString(c))
    }
    .via(reportErrorsFlow())

  private def authClientFlow(): Flow[Message, Message, Unit] =
    withWebsocketFlow(bs.authFlow())

  private def communicationFlow(sender: String): Flow[Message, Message, Unit] =
    withWebsocketFlow(bs.messageFlow(sender))

  private def reportErrorsFlow[A](): Flow[A, A, Unit] =
    Flow[A].transform { () => new PushStage[A, A] {
      override def onPush(elem: A, ctx: Context[A]) = ctx push elem
      override def onUpstreamFailure(cause: Throwable, ctx: Context[A]) = {
        Console.err.println(s"WebService stream failed with ${cause.getMessage}")
        cause.printStackTrace()
        super.onUpstreamFailure(cause, ctx)
      }
    }}
}
