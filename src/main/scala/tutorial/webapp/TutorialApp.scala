package tutorial.webapp

import scala.scalajs.js
import scala.scalajs.js.JSApp

import org.denigma.codemirror.CodeMirror
import org.denigma.codemirror.Editor
import org.denigma.codemirror.extensions.EditorConfig
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLTextAreaElement
import org.scalajs.jquery.jQuery

object TutorialApp extends JSApp {
  private val $ = jQuery
  private val jsc = js.Dynamic.global

  private object divs {
    val parent = "parent"
    val editorLeft = "editor_left"
    val editorRight = "editor_right"
    val render = "render"
    val right = "right"
    val left = "left"
  }

  override def main(): Unit = {
    $(setupUI _)
  }

  def setupDivs() = {
    import scalatags.JsDom.all._
    val par = div(id := divs.parent).render
    par.appendChild(div(
      id := divs.left,
      textarea(
        id := divs.editorLeft
      )
    ).render)
    par.appendChild(div(
      id := divs.right,
      textarea(id := divs.editorRight)
    ).render)
    par.appendChild(div(
      id := divs.render
    ).render)

    dom.document.body.appendChild(par)
  }

  def setupUI(): Unit = {
    setupDivs()

    val eLeft = setupEditor(divs.editorLeft, "text/x-scala").get
    eLeft.getDoc().setValue("""object O { val x = 0 }""")
    val eRight = setupEditor(divs.editorRight, "text/x-markdown").get
    eRight.getDoc().setValue("# Marked in browser\n\nRendered by **marked**.")

    def renderMarkdown(): Unit = {
      val markdown = eRight.getDoc().getValue()
      $(s"#${divs.render}").html(jsc.marked(markdown).toString())
    }

    eRight.on("keyup", (_: Editor) ⇒ renderMarkdown())

    $(s"#${divs.parent}").append("""
      <button id="click-me-button" type="button">Click me!</button>
    """)
    $("#click-me-button").click(addClickedMessage _)
    renderMarkdown()
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

  def addClickedMessage(): Unit = {
    $("body").append("<p>You clicked the button!</p>")
  }
}
