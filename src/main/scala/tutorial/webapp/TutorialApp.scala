package tutorial.webapp

import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document
import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.jQuery
import org.denigma.codemirror.extensions.EditorConfig
import org.denigma.codemirror.CodeMirror
import org.scalajs.dom.raw.HTMLTextAreaElement

object TutorialApp extends JSApp {
  val $ = jQuery

  override def main(): Unit = {
    $(setupUI _)
  }

  def setupUI(): Unit = {
    $("#click-me-button").click(addClickedMessage _)
    $("body").append("<p>Hello World</p>")
    val params = EditorConfig.mode("clike").lineNumbers(true)
    println(s">>>>> params: $params")
    val elem = dom.document.getElementById("scala").asInstanceOf[HTMLTextAreaElement]
    println(s">>>>> elem: $elem")
    val e = CodeMirror.fromTextArea(elem, params)
    println(s">>>>> e: $e")
    e.getDoc().setValue("""println("Hello Scala")""")
  }

  def addClickedMessage(): Unit = {
    $("body").append("<p>You clicked the button!</p>")
  }
}
