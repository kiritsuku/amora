package tutorial.webapp

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => jsg, newInstance => jsnew}
import scala.scalajs.js.JSApp

import org.denigma.codemirror.CodeMirror
import org.denigma.codemirror.Editor
import org.denigma.codemirror.extensions.EditorConfig
import org.scalajs.dom
import org.scalajs.dom.raw.ErrorEvent
import org.scalajs.dom.raw.Event
import org.scalajs.dom.raw.FocusEvent
import org.scalajs.dom.raw.HTMLTextAreaElement
import org.scalajs.dom.raw.KeyboardEvent
import org.scalajs.dom.raw.MessageEvent
import org.scalajs.dom.raw.MouseEvent
import org.scalajs.dom.raw.WebSocket
import org.scalajs.jquery.jQuery

import shared.test._

object TutorialApp extends JSApp {
  private val $ = jQuery

  private val ui = new Ui

  implicit class AsDynamic[A](private val a: A) extends AnyVal {
    def jsg: js.Dynamic = a.asInstanceOf[js.Dynamic]
  }

  private object divs {
    val parent = "parent"
    val editorLeft = "editor_left"
    val editorRight = "editor_right"
    val render = "render"
    val right = "right"
    val left = "left"
  }

  override def main(): Unit = {
    authenticate()
    setupUI2()
  }

  private val bm = new BufferManager
  private var ws: WebSocket = _
  private var clientName: String = _
  private var keyMap = Set[Int]()

  // used to measure running time of code
  private var startTime: js.Dynamic = _

  private val vimMap = Map(
    8  → "<bs>",
    9  → "<tab>",
//    13 → "<enter>",
//    16 → "<shift>",
//    17 → "<ctrl>",
//    18 → "<alt>",
    27 → "<esc>",
    32 → "<space>",
    33 → "<pageup>",
    34 → "<pagedown>",
    35 → "<end>",
    36 → "<home>",
    37 → "<left>",
    38 → "<up>",
    39 → "<right>",
    40 → "<down>",
    45 → "<ins>",
    46 → "<del>"
  )

  def setupUI2() = {
    import scalatags.JsDom.all._
    val par = div(id := divs.parent, `class` := "fullscreen").render
    val buf = bm.mkEditorBuf("text/x-scala")

    val b = ui.bufferDiv2(buf)
    par.appendChild(b)

    def start = b.selectionStart
    def end = b.selectionEnd

    def handleKeyUpDown(e: KeyboardEvent): Unit = {
      startTime = jsg.performance.now()
      val isDown = e.`type` == "keydown"
      keyMap = if (isDown) keyMap + e.keyCode else keyMap - e.keyCode

      if (isDown) {
        val controlSeq = vimMap.getOrElse(e.keyCode, "")
        if (controlSeq.nonEmpty) {
          // TODO don't create BufferRef manually here
          val input = Control(BufferRef(b.id), start, end, controlSeq)
          send(input)
          e.preventDefault()
        }
      }
    }

    def send(req: Request): Unit = {
      import boopickle.Default._
      val msg = Pickle.intoBytes(req)
      ws.send(toArrayBuffer(msg))
      println(s"> sent: $req")
    }

    def handleKeyPress(e: KeyboardEvent): Boolean = {
      startTime = jsg.performance.now()
      val character = jsg.String.fromCharCode(e.jsg.which).toString

      println(s"> $character")
      // TODO don't create BufferRef manually here
      val input = TextChange(BufferRef(b.id), start, end, character)
      send(input)

      false /* prevent default action */
    }

    def handleMouseUp(e: MouseEvent): Unit = {
      val input = SelectionChange(BufferRef(b.id), start, end)
      send(input)
    }

    b.onkeydown = handleKeyUpDown _
    b.onkeyup = b.onkeydown
    b.onkeypress = handleKeyPress _
    b.onblur = (_: FocusEvent) ⇒ keyMap = Set()
    b.onmouseup = handleMouseUp _

    $("body").append(par)
  }

  def setupDivs() = {
    import scalatags.JsDom.all._
    val par = div(id := divs.parent).render
    par appendChild ui.editorDiv(divs.left, divs.editorLeft)
    par appendChild ui.editorDiv(divs.right, divs.editorRight)
    par.appendChild(div(
      id := divs.render
    ).render)

    $("body").append(par)
  }

  def setupEditors() = {
    val eLeft = setupEditor(divs.editorLeft, "text/x-scala").get
    eLeft.getDoc().setValue("""object O { val x = 0 }""")
    val eRight = setupEditor(divs.editorRight, "text/x-markdown").get
    eRight.getDoc().setValue("# Marked in browser\n\nRendered by **marked**.")

    def renderMarkdown(): Unit = {
      val markdown = eRight.getDoc().getValue()
      $(s"#${divs.render}").html(jsg.marked(markdown).toString())
    }

    eRight.on("keyup", (_: Editor) ⇒ renderMarkdown())

    renderMarkdown()
  }

  def setupD3() = {
    import scalatags.JsDom._, svgTags._, svgAttrs._, implicits._

    val elem = svg(`class` := "svgtest", width := "200", height := "200").render
    val radi = Seq(40 -> "purple", 20 -> "green", 10 -> "red")
    for ((cur, curStyle) <- radi)
      elem appendChild circle(cx := 50, cy := 50, r := cur, style := s"fill:$curStyle;").render
    $("body").append(elem)
  }

  def authenticate() = {
    val ws = new WebSocket("ws://localhost:9999/auth")
    ws.binaryType = "arraybuffer"
    ws.onopen = (e: Event) ⇒ {
      println("connection opened")
    }
    ws.onerror = (e: ErrorEvent) => {
      dom.console.error(s"Couldn't create connection to server: $e")
    }
    ws.onmessage = (e: MessageEvent) ⇒ {
      import boopickle.Default._
      val bytes = toByteBuffer(e.data)
      val resp = Unpickle[Response].fromBytes(bytes)
      resp match {
        case ConnectionSuccessful(name) ⇒
          clientName = name
          println("connection successful")

          import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
          Future(setupWS())

        case ConnectionFailure ⇒
          println("connection failure")
          // TODO what to do here?

        case msg =>
          dom.console.error(s"Unexpected message: $msg")
      }
      ws.close()
    }
  }

  def setupWS() = {
    def websocketUri(name: String): String = {
      val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
      s"$wsProtocol://localhost:9999/communication?name=$name"
    }

    ws = new WebSocket(websocketUri(clientName))
    ws.binaryType = "arraybuffer"
    ws.onopen = (e: Event) ⇒ {
      println("connection opened")
    }
    ws.onerror = (e: ErrorEvent) => {
      dom.console.error(s"Couldn't create connection to server: $e")
    }
    ws.onmessage = (e: MessageEvent) ⇒ {
      import boopickle.Default._
      val bytes = toByteBuffer(e.data)
      val resp = Unpickle[Response].fromBytes(bytes)
      resp match {
        case ConnectionSuccessful(name) ⇒
          if (clientName != name) {
            // TODO what to do when this happens? It is probably a bug on the server
            dom.console.error(s"$clientName != $name")
            println("connection failure")
            clientName = null
          }
          else
            println("connection successful")

        case ConnectionFailure ⇒
          println("connection failure")
          // TODO what to do here?

        case InterpretedResult(id, res) ⇒
          val resultBuf = bm.resultBufOf(BufferRef(id)) // TODO remove BufferRef construction here
          println(s"retrieved interpreted result for id '$id', put it into '${resultBuf.ref.id}'")
          $(s"#${resultBuf.ref.id}").html(s"<pre><code>$res</code></pre>")
          mkEditor()

        case change @ TextChangeAnswer(bufferRef, start, end, text, cursor) ⇒
          println(s"> received: $change")
          val ta = dom.document.getElementById(bufferRef.id).asInstanceOf[HTMLTextAreaElement]

          val endTime = jsg.performance.now()
          val time = endTime.asInstanceOf[Double]-startTime.asInstanceOf[Double]
          println(s"update time: $time")
          ta.value = ta.value.substring(0, start) + text + ta.value.substring(end)
          ta.selectionStart = cursor
          ta.selectionEnd = ta.selectionStart

        case change @ SelectionChangeAnswer(bufferRef, start, end) ⇒
          println(s"> received: $change")
          val ta = dom.document.getElementById(bufferRef.id).asInstanceOf[HTMLTextAreaElement]

          ta.selectionStart = start
          ta.selectionEnd = end
      }
    }
    ws.onerror = (e: ErrorEvent) ⇒ {
      println(s"error from ws: $e")
    }
    ws.onclose = (e: Event) ⇒ {
      println(s"websocket closed")
    }
  }

  private def toByteBuffer(data: Any): ByteBuffer = {
    val ab = data.asInstanceOf[js.typedarray.ArrayBuffer]
    js.typedarray.TypedArrayBuffer.wrap(ab)
  }

  private def toArrayBuffer(data: ByteBuffer): js.typedarray.ArrayBuffer = {
    import js.typedarray.TypedArrayBufferOps._
    data.arrayBuffer
  }

  private def mkKeyMap(buf: Buffer): js.Object = {
    js.Dynamic.literal(
      "Ctrl-Enter" → { (e: Editor) ⇒
        mkResult(buf.ref)
        val resultBuf = bm.resultBufOf(buf.ref)
        // TODO replace by spinner
        $(s"#${resultBuf.ref.id}").html(s"<pre><code>...</code></pre>")

        import boopickle.Default._
        val code = e.getDoc().getValue()
        val msg = Pickle.intoBytes[Request](Interpret(buf.ref.id, code))
        ws.send(toArrayBuffer(msg))
      }
    )
  }

  def mkResult(editorRef: BufferRef): Unit = {
    val buf = bm.mkResultBuf(editorRef)

    val divType = ui.bufferDiv(buf) { editor ⇒
      editor.setSize("50%", "auto")
    }
    divType match {
      case DivType.Result(div) ⇒
        $("body").append(div)

      case res ⇒
        ??? // unexpected codepath
    }
  }

  def mkEditor(): Unit = {
    val buf = bm.mkEditorBuf("text/x-scala")

    val divType = ui.bufferDiv(buf) { editor ⇒
      editor.setSize("50%", "auto")
    }

    divType match {
      case DivType.Editor(div, editor) ⇒
        editor.addKeyMap(mkKeyMap(buf))
        $("body").append(div)

      case res ⇒
        ??? // unexpected codepath
    }
  }

  def setupTheButton() = {
    import scalatags.JsDom.all._

    val b = button(id := "click-me-button", `type` := "button", "Click me!").render
    b.onclick = (_: MouseEvent) ⇒ {
      mkEditor()
    }
    $(s"#${divs.parent}").append(b)
  }

  def setupUI(): Unit = {
    setupDivs()
    setupEditors()
    setupD3()
    setupTheButton()
    authenticate()
  }

  def setupEditor(id: String, mode: String): Option[Editor] = {
    dom.document.getElementById(id) match {
      case elem: HTMLTextAreaElement ⇒
        val params = EditorConfig.mode(mode).lineNumbers(true).theme("solarized")
        Some(CodeMirror.fromTextArea(elem, params))
      case elem ⇒
        Console.err.println(s"unexpected element: $elem")
        None
    }
  }
}
