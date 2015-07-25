package tutorial.webapp

import java.nio.ByteBuffer

import scala.scalajs.js
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

import shared.test.Person

object TutorialApp extends JSApp {
  private val $ = jQuery
  private val jsc = js.Dynamic.global

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

  private var editors = Map[String, AEditor]()
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
      $(s"#${divs.render}").html(jsc.marked(markdown).toString())
    }

    eRight.on("keyup", (_: Editor) ⇒ renderMarkdown())

    renderMarkdown()
  }

  def setupWS() = {
    def websocketUri(name: String): String = {
      val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
      s"$wsProtocol://localhost:9999/communication?name=$name"
    }

    def toByteBuffer(data: Any): ByteBuffer = {
      val ab = data.asInstanceOf[js.typedarray.ArrayBuffer]
      println("message size: " + ab.byteLength)
      js.typedarray.TypedArrayBuffer.wrap(ab)
    }

    ws = new WebSocket(websocketUri("client1"))
    ws.binaryType = "arraybuffer"
    ws.onopen = (e: Event) ⇒ {
      println("connection opened")
    }
    ws.onmessage = (e: MessageEvent) ⇒ {
      import boopickle.Default._
      val bytes = toByteBuffer(e.data)
      val persons = Unpickle[Seq[Person]].fromBytes(bytes)
      println(persons)
    }
    ws.onerror = (e: ErrorEvent) ⇒ {
      println(s"error from ws: $e")
    }
    ws.onclose = (e: Event) ⇒ {
      println(s"websocket closed")
    }
  }

  def setupTheButton() = {
    import scalatags.JsDom.all._

    val b = button(id := "click-me-button", `type` := "button", "Click me!").render
    b.onclick = (_: MouseEvent) ⇒ {
      ws.send("the message")

      val name = "editor"+editors.size
      val editor = ui.editorDiv(name, s"$name-ta", "text/x-scala")

      editors += name → editor
      val r = div(id := s"$name-outer", editor.editorDiv, editor.resultDiv).render
      editor.editor.on("keyup", (e: Editor) ⇒ {
        val code = e.getDoc().getValue()
        editor.resultDiv.innerHTML = code
      })
      $("body").append(r)
    }
    $(s"#${divs.parent}").append(b)
    $("body").append(shared.test.Shared.func(5))
  }

  def setupUI(): Unit = {
    setupDivs()
    setupEditors()
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
