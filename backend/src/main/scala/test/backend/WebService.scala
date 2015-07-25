package test.backend

import java.io.PrintWriter
import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.BinaryMessage
import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.model.ws.TextMessage
import akka.http.scaladsl.server.Directives
import akka.stream.Materializer
import akka.stream.scaladsl.Flow
import akka.util.CompactByteString
import shared.test.Person
import akka.stream.stage.PushStage
import akka.stream.stage.Context

class WebService(implicit m: Materializer, system: ActorSystem) extends Directives {

  private val bs = new BackendSystem()

  private def fromWebjar(path: String) =
    getFromResource(s"/home/antoras/dev/scala/scalajs-test/ui/target/web/web-modules/main/webjars/lib/$path")

  def route =
    get {
      pathSingleSlash(getFromResource("index.html")) ~
      path("scalajs-test-ui-jsdeps.js")(getFromResource("scalajs-test-ui-jsdeps.js")) ~
      path("scalajs-test-ui-fastopt.js")(getFromResource("scalajs-test-ui-fastopt.js")) ~
      path("scalajs-test-ui-launcher.js")(getFromResource("scalajs-test-ui-launcher.js")) ~
      //path("marked.js")(fromWebjar("marked/lib/marked.js")) ~
      path("marked.js"){
        parameter('name) { name ⇒
          println(">>>>" + name)
          fromWebjar("marked/lib/marked.js")
        }
      } ~
      path("clike.js")(fromWebjar("codemirror/mode/clike/clike.js")) ~
      path("markdown.js")(fromWebjar("codemirror/mode/markdown/markdown.js")) ~
      path("codemirror.css")(fromWebjar("codemirror/lib/codemirror.css")) ~
      path("solarized.css")(fromWebjar("codemirror/theme/solarized.css")) ~
      path("communication") {
        parameter('name) { name ⇒
          handleWebsocketMessages(websocketFlow(sender = name))
        }
      }
    }

  def websocketFlow(sender: String): Flow[Message, Message, Unit] =
    Flow[Message]
    .collect {
      case TextMessage.Strict(msg) ⇒ msg
    }
    .via(bs.messageFlow(sender))
    .map {
      case c ⇒ BinaryMessage(CompactByteString(c))
    }
    .via(reportErrorsFlow)

  def reportErrorsFlow[A]: Flow[A, A, Unit] =
    Flow[A].transform { () => new PushStage[A, A] {
      override def onPush(elem: A, ctx: Context[A]) = ctx push elem
      override def onUpstreamFailure(cause: Throwable, ctx: Context[A]) = {
        Console.err.println(s"WebService stream failed with ${cause.getMessage}")
        cause.printStackTrace()
        super.onUpstreamFailure(cause, ctx)
      }
    }}
}
