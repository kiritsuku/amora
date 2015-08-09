package tutorial.webapp

import java.nio.ByteBuffer
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => jsg, newInstance => jsnew}
import scala.scalajs.js.JSApp
import org.denigma.codemirror.CodeMirror
import org.denigma.codemirror.Editor
import org.denigma.codemirror.extensions.EditorConfig
import org.scalajs.dom
import org.scalajs.dom.raw.ErrorEvent
import org.scalajs.dom.raw.Event
import org.scalajs.dom.raw.HTMLTextAreaElement
import org.scalajs.dom.raw.MessageEvent
import org.scalajs.dom.raw.MouseEvent
import org.scalajs.dom.raw.WebSocket
import org.scalajs.jquery.jQuery
import shared.test.Execute
import shared.test.Interpret
import shared.test.Person
import shared.test.Request
import shared.test.Response
import shared.test.InterpretedResult
import shared.test.PersonList
import shared.test.Person

object TutorialApp extends JSApp {
  private val $ = jQuery

  private val ui = new Ui

  private object divs {
    val parent = "parent"
    val editorLeft = "editor_left"
    val editorRight = "editor_right"
    val render = "render"
    val right = "right"
    val left = "left"
  }

  override def main(): Unit = {
    setupUI()
  }

  private val bm = new BufferManager
  private var ws: WebSocket = _

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

  def setupWS() = {
    def websocketUri(name: String): String = {
      val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
      s"$wsProtocol://localhost:9999/communication?name=$name"
    }

    ws = new WebSocket(websocketUri("client1"))
    ws.binaryType = "arraybuffer"
    ws.onopen = (e: Event) ⇒ {
      println("connection opened")
    }
    ws.onmessage = (e: MessageEvent) ⇒ {
      import boopickle.Default._
      val bytes = toByteBuffer(e.data)
      val resp = Unpickle[Response].fromBytes(bytes)
      resp match {
        case InterpretedResult(id, res) ⇒
          val resultBuf = bm.resultBufOf(BufferRef(id)) // TODO remove BufferRef construction here
          println(s"retrieved interpreted result for id '$id', put it into '${resultBuf.ref.id}'")
          $(s"#${resultBuf.ref.id}").html(s"<pre><code>$res</code></pre>")
        case PersonList(persons) ⇒
          println(s"retrieved persons: $persons")
        case p: Person ⇒
          println(s"retrieved person: $p")
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

        import boopickle.Default._
        val code = e.getDoc().getValue()
        val msg = Pickle.intoBytes[Request](Interpret(buf.ref.id, code))
        ws.send(toArrayBuffer(msg))
      }
    )
  }

  def mkResult(editorRef: BufferRef): Unit = {
    val buf = bm.mkResultBuf(editorRef)

    val divType = ui.bufferDiv(buf)
    divType match {
      case DivType.Result(div) ⇒
        $("body").append(div)

      case res ⇒
        ??? // unexpected codepath
    }
  }

  def mkEditor(): Unit = {
    val buf = bm.mkEditorBuf("text/x-scala")

    val divType = ui.bufferDiv(buf)
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
    setupWS()
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
