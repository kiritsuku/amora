package frontend.webui

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.JSON

import org.scalajs.dom
import org.scalajs.dom.raw.ErrorEvent
import org.scalajs.dom.raw.Event
import org.scalajs.dom.raw.MessageEvent
import org.scalajs.dom.raw.WebSocket

import frontend.webui.protocol.Response
import protocol.ConnectionFailure
import protocol.ConnectionSuccessful

object Main extends JSApp {

  implicit class AsDynamic[A](private val a: A) extends AnyVal {
    def jsg: js.Dynamic = a.asInstanceOf[js.Dynamic]
  }

  val ServerAddress = "localhost:9999"

  /** The socket to the server */
  private var ws: WebSocket = _
  /** The ID of the client which is assigned by the server after authentication. */
  private var id: String = _

  override def main(): Unit = {
    authenticate()
  }

  def authenticate() = {
    val ws = new WebSocket(websocketUri("auth-web"))
    ws.binaryType = "arraybuffer"
    ws.onopen = (e: Event) ⇒ {
      dom.console.info("Connection for server authentication opened")
    }
    ws.onerror = (e: ErrorEvent) ⇒ {
      dom.console.error(s"Couldn't create connection to server: ${JSON.stringify(e)}")
    }
    ws.onmessage = (e: MessageEvent) ⇒ {
      import boopickle.Default._
      val bytes = toByteBuffer(e.data)
      Unpickle[Response].fromBytes(bytes) match {
        case ConnectionSuccessful(id) ⇒
          dom.console.info(s"Server assigned id `$id`.")
          this.id = id

          import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
          Future(setupWS())

        case ConnectionFailure(reason) ⇒
          dom.console.error(s"Server rejected connection request: $reason")

        case msg ⇒
          dom.console.error(s"Unexpected message arrived: $msg")
      }
      ws.close()
    }
  }

  def setupWS() = {
    ws = new WebSocket(websocketUri("kb"))
    ws.binaryType = "arraybuffer"
    ws.onopen = (e: Event) ⇒ {
      dom.console.info("Connection for server communication opened")
    }
    ws.onerror = (e: ErrorEvent) ⇒ {
      dom.console.error(s"Couldn't create connection to server: ${JSON.stringify(e)}")
    }
    ws.onmessage = (e: MessageEvent) ⇒ {
      import boopickle.Default._
      val bytes = toByteBuffer(e.data)
      handleResponse(Unpickle[Response].fromBytes(bytes))
    }
    ws.onclose = (e: Event) ⇒ {
      dom.console.info(s"websocket for server communication closed")
      ws = null
    }
  }

  def handleResponse(response: Response) = response match {
    case msg ⇒
      dom.console.error(s"Unexpected message arrived: $msg")
  }

  private def websocketUri(path: String): String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    s"$wsProtocol://$ServerAddress/$path"
  }

  private def toByteBuffer(data: Any): ByteBuffer = {
    val ab = data.asInstanceOf[js.typedarray.ArrayBuffer]
    js.typedarray.TypedArrayBuffer.wrap(ab)
  }
}
