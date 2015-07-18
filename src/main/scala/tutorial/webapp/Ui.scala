package tutorial.webapp

import org.scalajs.dom
import org.denigma.codemirror.CodeMirror
import org.denigma.codemirror.Editor
import org.denigma.codemirror.extensions.EditorConfig
import org.scalajs.dom.raw.HTMLTextAreaElement

class Ui {
  import scalatags.JsDom.all
  import scalatags.JsDom.all._

  def editorDiv(divId: String, taId: String) = {
    div(id := divId, textarea(id := taId)).render
  }

  def editorDiv(
      divId: String,
      editorId: String,
      editorMode: String
  ): AEditor = {
    val ta = textarea(id := editorId).render.asInstanceOf[HTMLTextAreaElement]
    val d = div(id := divId, ta).render

    val params = EditorConfig.mode(editorMode).theme("solarized")
    val e = CodeMirror.fromTextArea(ta, params)
    e.setSize("50%", 50)
    
    val r = resultDiv(s"$divId-result")
    AEditor(d, r, e)
  }
  
  def resultDiv(
      divId: String) = {
    val d = div(id := divId).render
    d
  }

}

case class AEditor(editorDiv: dom.html.Div, resultDiv: dom.html.Div, editor: Editor)
