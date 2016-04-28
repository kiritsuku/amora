package frontend.webui

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.JSON

import org.scalajs.dom
import org.scalajs.dom.raw._

import frontend.webui.protocol._
import frontend.webui.protocol.QueueItem

object Main extends JSApp {
  private val $ = org.scalajs.jquery.jQuery

  implicit class AsDynamic[A](private val a: A) extends AnyVal {
    def jsg: js.Dynamic = a.asInstanceOf[js.Dynamic]
  }

  val ServerAddress = "localhost:9999"

  /** The socket to the server */
  private var ws: WebSocket = _
  /** The ID of the client which is assigned by the server after authorization. */
  private var clientId: String = _

  override def main(): Unit = {
    authorize()
  }

  def authorize() = {
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

          import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
          Future(setupWS())

        case msg ⇒
          dom.console.error(s"Unexpected message arrived: $msg")
      }
      ws.close()
      dom.console.info("Connection for client authorization closed")
    }
  }

  def setupWS() = {
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
      handleResponse(Unpickle[Response].fromBytes(bytes))
    }
    ws.onclose = (e: CloseEvent) ⇒ {
      val reason = if (e.reason.isEmpty) "" else s" Reason: ${e.reason}"
      dom.console.info(s"Connection for server communication closed.$reason")
      ws = null
    }
  }

  def handleResponse(response: Response) = response match {
    case ConnectionSuccessful ⇒
      dom.console.info(s"Connection to server established. Communication is now possible.")
      showMainPage()

    case req: QueueItems ⇒
      handleQueueItems(req)

    case req: QueueItem ⇒
      handleQueueItem(req)

    case msg ⇒
      dom.console.error(s"Unexpected message arrived: $msg")
  }

  def showMainPage() = {
    import scalatags.JsDom.all._

    val content = div(
        h3("Knowledge Base"),
        ul(
          li(id := "li1", a(href := "", "Show queue", onclick := "return false;"))
        ),
        div(id := "content")
    ).render
    $("body").append(content)

    handleClickEvent("li1")(_ ⇒ send(GetQueueItems))
  }

  def handleQueueItems(req: QueueItems) = {
    import scalatags.JsDom.all._
    val content = div(
      h4("Queue Items"),
      ul(
        if (req.items.isEmpty)
          li("No items")
        else
          for (i ← req.items) yield li(id := s"item$i", a(href := "", s"Item $i", onclick := "return false;"))
      )
    ).render
    $("#content").empty().append(content)

    for (i ← req.items) handleClickEvent(s"item$i")(_ ⇒ send(GetQueueItem(i)))
  }

  def handleQueueItem(req: QueueItem) = {
    import scalatags.JsDom.all._
    if (req.appendLog) {
      val d = dom.document.getElementById(s"item${req.id}").asInstanceOf[dom.html.TextArea]
      d.value += req.log
    } else {
      val content = div(
        h4(s"Queue Item ${req.id}"),
        textarea(id := s"item${req.id}", req.log)
      ).render
      $("#content").empty().append(content)
    }
  }

  def send(req: Request): Unit = {
    import boopickle.Default._
    val msg = Pickle.intoBytes(req)
    ws.send(toArrayBuffer(msg))
    dom.console.info(s"Sent request: $req")
  }

  private def handleClickEvent(id: String)(f: MouseEvent ⇒ Unit) = {
    val d = dom.document.getElementById(id).asInstanceOf[dom.html.Link]
    d.onclick = (e: MouseEvent) ⇒ f(e)
  }

  private def websocketUri(path: String): String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    s"$wsProtocol://$ServerAddress/$path"
  }

  private def toByteBuffer(data: Any): ByteBuffer = {
    val ab = data.asInstanceOf[js.typedarray.ArrayBuffer]
    js.typedarray.TypedArrayBuffer.wrap(ab)
  }

  private def toArrayBuffer(data: ByteBuffer): js.typedarray.ArrayBuffer = {
    import js.typedarray.TypedArrayBufferOps._
    data.arrayBuffer
  }
}
