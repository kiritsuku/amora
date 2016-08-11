package frontend.webui

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom
import org.scalajs.dom.raw._

import frontend.webui.protocol._
import frontend.webui.protocol.QueueItem

@JSExport
object Main extends JSApp {
  private val $ = org.scalajs.jquery.jQuery

  implicit class AsDynamic[A](private val a: A) extends AnyVal {
    def jsg: js.Dynamic = a.asInstanceOf[js.Dynamic]
  }

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
          Future {
            setupWS()
            showMainPage()
          }

        case msg ⇒
          dom.console.error(s"Unexpected message arrived: $msg")
      }
      ws.close()
      dom.console.info("Connection for client authorization closed")
    }
  }

  def setupWS(): Unit = {
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
      handleResponse(resp)
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

  def handleResponse(response: Response) = response match {
    case ConnectionSuccessful ⇒
      dom.console.info(s"Connection to server established. Communication is now possible.")

    case resp: QueueItems ⇒
      handleQueueItems(resp)

    case resp: QueueItem ⇒
      handleQueueItem(resp)

    case resp: Schemas ⇒
      handleSchemas(resp)

    case resp: Schema ⇒
      handleSchema(resp)

    case resp: RequestSucceeded ⇒
      handleRequestSucceeded(resp)

    case resp: RequestFailed ⇒
      handleRequestFailed(resp)

    case msg ⇒
      dom.console.error(s"Unexpected message arrived: $msg")
  }

  def showMainPage() = {
    import scalatags.JsDom.all._

    val content = div(
        h3("Knowledge Base"),
        ul(
          li(id := "li1", a(href := "", "Show queue", onclick := "return false;")),
          li(id := "li2", a(href := "", "Show schemas", onclick := "return false;"))
        ),
        div(id := "content")
    ).render
    $("body").append(content)

    handleClickEvent("li1")(_ ⇒ send(GetQueueItems))
    handleClickEvent("li2")(_ ⇒ send(GetSchemas))
  }

  def handleRequestSucceeded(succ: RequestSucceeded) = {
    import scalatags.JsDom.all._

    val content = div(style := "background-color: green", raw(succ.msg)).render
    $("#content").empty().append(content)
  }

  def handleRequestFailed(fail: RequestFailed) = {
    import scalatags.JsDom.all._

    val content = div(style := "background-color: red", raw(fail.msg)).render
    $("#content").empty().append(content)
  }

  def handleQueueItems(items: QueueItems) = {
    import scalatags.JsDom.all._
    val content = div(
      h4("Queue Items"),
      ul(
        if (items.items.isEmpty)
          li("No items")
        else
          for (i ← items.items) yield li(id := s"item$i", a(href := "", s"Item $i", onclick := "return false;"))
      )
    ).render
    $("#content").empty().append(content)

    for (i ← items.items) handleClickEvent(s"item$i")(_ ⇒ send(GetQueueItem(i)))
  }

  def handleQueueItem(item: QueueItem) = {
    import scalatags.JsDom.all._
    if (item.appendLog) {
      val d = dom.document.getElementById(s"item${item.id}").asInstanceOf[dom.html.TextArea]
      d.value += item.log
    } else {
      val content = div(
        h4(s"Queue Item ${item.id}"),
        textarea(id := s"item${item.id}", rows := "20", cols := "150", item.log)
      ).render
      $("#content").empty().append(content)
    }
  }

  def handleSchemas(schemas: Schemas) = {
    import scalatags.JsDom.all._
    val content = div(
      h4(s"Schemas"),
      select(
        id := "schemas",
        for (schemaName ← schemas.schemaNames) yield
          if (schemaName == schemas.defaultSchema.name)
            option(selected := "", schemaName)
          else
            option(schemaName)
      ),
      div(id := "schema")
    ).render
    $("#content").empty().append(content)

    handleSchema(schemas.defaultSchema)
    val d = dom.document.getElementById("schemas").asInstanceOf[dom.html.Select]
    d.onchange = (_: Event) ⇒ {
      val selectedSchema = d.options(d.selectedIndex).textContent
      send(GetSchema(selectedSchema))
    }
  }

  // TODO Get rid of this @JSExport
  // It is a hack which was needed to call the Scala code from Alpaca.js
  @JSExport
  def handleFormSubmit(elem: js.Object) = {
    val value = elem.jsg.getValue()
    val formattedJson = JSON.stringify(value, null: js.Function2[String, js.Any, js.Any], "  ")
    send(IndexData(formattedJson))
  }

  def handleSchema(schema: Schema) = {
    import scalatags.JsDom.all._
    val content = div(
      div(id := "schemaForm"),
      // TODO Replace this JS script with Scala code
      // We should use `$("#schemaForm").jsg.alpaca(JSON.parse(schema.jsonSchema))`
      // but we can't yet because the json schema contains non JSON code but some
      // JS definitions which Scala.js doesn't understand. Once we got rid with the
      // JS definitions, we can also fix this issue.
      script(`type` := "text/javascript", raw(s"""
        $$("#schemaForm").alpaca(${schema.jsonSchema});
      """))
    ).render
    $("#schema").empty().append(content)
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
