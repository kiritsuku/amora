package tutorial.webapp

import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document
import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.jQuery
import org.denigma.codemirror.extensions.EditorConfig
import org.denigma.codemirror.CodeMirror
import org.scalajs.dom.raw.HTMLTextAreaElement
import org.denigma.codemirror.Editor

object TutorialApp extends JSApp {
  val $ = jQuery

  override def main(): Unit = {
    $(setupUI _)
  }

  def setupUI(): Unit = {
    val parent = "parent"
    val editorLeft = "editor_left"
    val editorRight = "editor_right"

    $("body").prepend(s"""<div id="$parent"></div>""")
    $(s"#$parent").append(s"""
      <div id="left">
        <textarea id="$editorLeft"></textarea>
      </div>
    """)
    $(s"#$parent").append(s"""
      <div id="right">
        <textarea id="$editorRight"></textarea>
      </div>
    """)
    val e = setupEditor(editorLeft)
    e.get.getDoc().setValue("""object O { val x = 0 }""")
    val e2 = setupEditor(editorRight)
    e2.get.getDoc().setValue("""var x = 0""")
    $("#click-me-button").click(addClickedMessage _)
  }

  def setupEditor(id: String): Option[Editor] = {
    dom.document.getElementById(id) match {
      case elem: HTMLTextAreaElement ⇒
        val params = EditorConfig.mode("text/x-scala").lineNumbers(true).theme("solarized")
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
