package ui

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => jsg, newInstance => jsnew}

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

import protocol._

object Ui {

  implicit class AsDynamic[A](private val a: A) extends AnyVal {
    def jsg: js.Dynamic = a.asInstanceOf[js.Dynamic]
  }

}

class Ui {
  import Ui._

  private val $ = jQuery
  private val gen = new DomGen

  private object divs {
    val parent = "parent"
    val editorLeft = "editor_left"
    val editorRight = "editor_right"
    val render = "render"
    val right = "right"
    val left = "left"
  }

  private val bm = new BufferManager
  private var ws: WebSocket = _
  private var clientName: String = _
  private var keyMap = Set[Int]()
  private var windows = Set[Int]()

  // used to measure running time of code
  private var startTime: js.Dynamic = _

  private val vimMap = Map(
    8  → "<bs>",
    9  → "<tab>",
//    13 → "enter",
//    16 → "shift",
//    17 → "C",
//    18 → "alt",
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

  def setupUI3() = {
  }

  def createBufferContent(buf: Buffer) = {
    println(s"Create new DOM for buffer `${buf.ref.id}`")
    import scalatags.JsDom.all._

    val par = div(id := divs.parent, `class` := "fullscreen").render
    val d = gen.bufferDiv3(buf, tabIndex = 1)
    val ta = textarea(id := s"${buf.ref.id}-ta", style := "display: none;").render

    d.appendChild(ta)
    par.appendChild(d)

    def handleKeyUpDown(e: KeyboardEvent): Unit = {
      println("keyUpDown: " + e.keyCode)
      startTime = jsg.performance.now()
      val isDown = e.`type` == "keydown"
      keyMap = if (isDown) keyMap + e.keyCode else keyMap - e.keyCode

      def isCtrlPressed = keyMap.contains(17)

      if (isDown) {
        val controlSeq = vimMap.getOrElse(e.keyCode, "")
        if (controlSeq.nonEmpty) {
          val input = Control(buf.ref.id, controlSeq)
          send(input)
          e.preventDefault()
        } else if (isCtrlPressed && e.keyCode != 17) {
          val character = jsg.String.fromCharCode(e.jsg.which).toString
          val input = Control(buf.ref.id, s"<C-$character>")
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
      println("keyPress: " + e.keyCode)
      startTime = jsg.performance.now()
      val character = jsg.String.fromCharCode(e.jsg.which).toString

      val input = TextChange(buf.ref.id, character)
      send(input)

      false /* prevent default action */
    }

    def handleMouseUp(e: MouseEvent): Unit = {
      startTime = jsg.performance.now()
      val sel = selection
      val start = offsetToVimPos(sel._1)
      val input = SelectionChange(buf.ref, start._1, start._2)
      send(input)
    }

    /* Returns the selection as (start, end). */
    def selection: (Int, Int) = {
      val sel = dom.window.getSelection()
      val range = sel.getRangeAt(0)
      val elem = dom.document.getElementById(buf.ref.id.toString)
      val content = range.cloneRange()
      content.selectNodeContents(elem)
      content.setEnd(range.startContainer, range.startOffset)
      val start = content.toString().length()

      content.setStart(range.startContainer, range.startOffset)
      content.setEnd(range.endContainer, range.endOffset)
      val len = content.toString().length()
      (start, start+len)
    }

    def offsetToVimPos(offset: Int): (Int, Int) = {
      val elem = dom.document.getElementById(buf.ref.id.toString)
      val content = elem.textContent.substring(0, offset)
      val row = content.count(_ == '\n')
      val col = offset-content.lastIndexWhere(_ == '\n')-1
      (row, col)
    }

    d.onkeydown = handleKeyUpDown _
    d.onkeyup = d.onkeydown
    d.onkeypress = handleKeyPress _
    d.onblur = (_: FocusEvent) ⇒ keyMap = Set()
    d.onmouseup = handleMouseUp _

    $("body").append(par)
    $(s"#${buf.ref.id}").focus()
  }

  def setupDivs() = {
    import scalatags.JsDom.all._
    val par = div(id := divs.parent).render
    par appendChild gen.editorDiv(divs.left, divs.editorLeft)
    par appendChild gen.editorDiv(divs.right, divs.editorRight)
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
          val resultBuf = bm.resultBufOf(BufferRef(id.toInt)) // TODO remove BufferRef construction here
          println(s"retrieved interpreted result for id '$id', put it into '${resultBuf.ref.id}'")
          $(s"#${resultBuf.ref.id}").html(s"<pre><code>$res</code></pre>")
          mkEditor()

        case change @ TextChangeAnswer(bufferRef, lines, sel) ⇒
          println(s"> received: $change")
          updateBuffer(bufferRef.id, lines)
          updateCursor(bufferRef, sel)

          val endTime = jsg.performance.now()
          val time = endTime.asInstanceOf[Double]-startTime.asInstanceOf[Double]
          println(s"update time: $time")

        case change @ SelectionChangeAnswer(bufferRef, sel) ⇒
          println(s"> received: $change")
          updateCursor(bufferRef, sel)

          val endTime = jsg.performance.now()
          val time = endTime.asInstanceOf[Double]-startTime.asInstanceOf[Double]
          println(s"update time: $time")

        case update @ ClientUpdate(winId, bufferId, mode, lines, sel) ⇒
          println(s"> received: $update")

          // TODO remove BufferRef creation here
          val buf = bm.bufferOf(BufferRef(bufferId))
          if (!windows(winId)) {
            windows += winId
            createBufferContent(buf)
          }

          buf.mode = mode
          updateBuffer(buf.ref.id, lines)
          updateCursor(buf.ref, sel)

        case ev ⇒
          dom.console.error(s"Unexpected response: $ev")
      }
    }

    def vimPosToOffset(ref: BufferRef, row: Int, col: Int): Int = {
      val elem = dom.document.getElementById(ref.id.toString)
      val lines = elem.textContent.split("\n")
      val nrOfCharsBeforeCursor =
        if (row == 0)
          0
        else
          lines.take(row).map(_.length).sum+row

      nrOfCharsBeforeCursor+col
    }

    def updateCursor(bufferRef: BufferRef, sel: Selection): Unit = {
      val offset = vimPosToOffset(bufferRef, sel.start.row, sel.start.col)
      val bsel = dom.window.getSelection()
      val range = bsel.getRangeAt(0)
      range.setStart(range.startContainer, offset)

      if (sel.start.row != sel.end.row || sel.start.col != sel.end.col) {
        val offset = vimPosToOffset(bufferRef, sel.end.row, sel.end.col)
        range.setEnd(range.startContainer, offset)
      }
      else
        range.setEnd(range.startContainer, offset)

      bsel.removeAllRanges()
      bsel.addRange(range)
    }

    def updateBuffer(bufferId: Int, lines: Seq[String]): Unit = {
      val parent = dom.document.getElementById(bufferId.toString)
      val content = lines.mkString("\n")
      parent.innerHTML = content
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

        /*
        val opts = js.Dynamic.literal(
          color = "#fff",
          lines = 12
        )
        jsnew(jsg.Spinner)(opts).spin(dom.document.getElementById(resultBuf.ref.id))
        */

        import boopickle.Default._
        val code = e.getDoc().getValue()
        val msg = Pickle.intoBytes[Request](Interpret(buf.ref.id.toString, code))
        ws.send(toArrayBuffer(msg))
      }
    )
  }

  def mkResult(editorRef: BufferRef): Unit = {
    val buf = bm.mkResultBuf(editorRef)

    val divType = gen.bufferDiv(buf) { editor ⇒
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

    val divType = gen.bufferDiv(buf) { editor ⇒
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
    /*
        val d = div(id := "hello").render
        $(s"#${divs.parent}").append(d)
        val opts = js.Dynamic.literal(
          color = "#fff",
          lines = 12,
          width = 3,
          length = 6,
          radius = 5,
          top = "5%",
          left = "5%",
          position = "relative"
        )
        jsnew(jsg.Spinner)(opts).spin(dom.document.getElementById("hello"))
    */
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
