package amora.frontend.webui

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.JSON

import org.scalajs.dom
import org.scalajs.dom.raw.CloseEvent
import org.scalajs.dom.raw.ErrorEvent
import org.scalajs.dom.raw.Event
import org.scalajs.dom.raw.MessageEvent
import org.scalajs.dom.raw.WebSocket

import amora.frontend.webui.protocol.AuthorizationGranted
import amora.frontend.webui.protocol.Request
import amora.frontend.webui.protocol.Response

/**
 * Maintains a web socket connection to the server. The connection is created by
 * calling the [[setup]] method. The constructor takes a response handler, which
 * gets notified for every response that arrives from the server.
 */
class Connection(responseHandler: Response ⇒ Unit) {

  /** The socket to the server */
  private var ws: WebSocket = _

  /** The ID of the client which is assigned by the server after authorization. */
  private var clientId: String = _

  /**
   * Creates the connection to the server.
   */
  def setup(): Unit = {
    authorize()
  }

  /**
   * Sends a request to the server.
   */
  def send(req: Request): Unit = {
    import boopickle.Default._
    val msg = Pickle.intoBytes(req)
    ws.send(toArrayBuffer(msg))
    dom.console.info(s"Sent request: $req")
  }

  private def authorize() = {
    val ws = new WebSocket(websocketUri("auth-web"))
    ws.binaryType = "arraybuffer"
    ws.onopen = (e: Event) ⇒ {
      dom.console.info("Connection for client authorization opened")
    }
    ws.onerror = (e: ErrorEvent) ⇒ {
      dom.console.error(s"Couldn't create connection to server: ${JSON.stringify(e)}")
    }
    ws.onmessage = (e: MessageEvent) ⇒ {
      import boopickle.Default._
      val bytes = toByteBuffer(e.data)
      Unpickle[Response].fromBytes(bytes) match {
        case AuthorizationGranted(id) ⇒
          dom.console.info(s"Server assigned id `$id`.")
          this.clientId = id

          Future {
            setupWS()
          } onFailure {
            case t ⇒
              throw t
          }

        case msg ⇒
          dom.console.error(s"Unexpected message arrived: $msg")
      }
      ws.close()
      dom.console.info("Connection for client authorization closed")
    }
  }

  private def setupWS(): Unit = {
    ws = new WebSocket(websocketUri(s"kbws?id=$clientId"))
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
      val resp = Unpickle[Response].fromBytes(bytes)
      dom.console.info(s"Received response from server: $resp")
      responseHandler(resp)
    }
    ws.onclose = (e: CloseEvent) ⇒ {
      val reason = if (e.reason.isEmpty) "" else s" Reason: ${e.reason}"
      dom.console.info(s"Connection for server communication closed.$reason")
      dom.window.setTimeout({ () ⇒
        // when the connection is closed automatically, we want to reconnect
        dom.console.info("Recreating connection for server communication")
        setupWS()
      }, 100)
    }
  }

  private def websocketUri(path: String): String = {
    // The server address is defined by the server
    val addr = js.Dynamic.global.ServerAddress.toString
    val wsAddr =
      if (addr.startsWith("https:"))
        addr.replaceFirst("https:", "wss:")
      else if (addr.startsWith("http:"))
        addr.replaceFirst("http:", "ws:")
      else
        throw new Exception(s"Invalid server address: $addr. It needs to start with https or http.")
    val fullAddr = s"$wsAddr/$path"

    dom.console.log(s"Connecting to `$fullAddr`")
    fullAddr
  }

  private def toByteBuffer(data: Any): ByteBuffer = {
    val ab = data.asInstanceOf[js.typedarray.ArrayBuffer]
    js.typedarray.TypedArrayBuffer.wrap(ab)
  }

  private def toArrayBuffer(data: ByteBuffer): js.typedarray.ArrayBuffer = {
    import scala.scalajs.js.typedarray.TypedArrayBufferOps._
    data.arrayBuffer
  }
}
