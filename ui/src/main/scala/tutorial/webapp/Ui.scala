package tutorial.webapp

import org.scalajs.dom
import org.denigma.{codemirror ⇒ cm}
import scala.scalajs.js.Dynamic.{global => jsg}
import org.denigma.codemirror.CodeMirror
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

  def bufferDiv2(buf: Buffer) = {
    val bid = buf.ref.id
    val ta = textarea(id := bid, `class` := "fullscreen", style := "resize: none;").render.asInstanceOf[HTMLTextAreaElement]
    ta
  }

  def bufferDiv(buf: Buffer)(f: cm.Editor ⇒ Unit): DivType.DivType = {
    val divId = buf.ref.id

    def mkEditorDiv(editorMode: String) = {
      def enterVimMode(e: cm.Editor) = {
        e.setOption("disableInput", true)
        e.setOption("showCursorWhenSelecting", false)
      }
      def leaveVimMode(e: cm.Editor) = {
        e.setOption("disableInput", false)
        e.setOption("showCursorWhenSelecting", true)
      }
      def attach(e: cm.Editor) = {
        jsg.CodeMirror.addClass(e.getWrapperElement(), "cm-fat-cursor")
        enterVimMode(e)
      }
      def detach(e: cm.Editor) = {
        jsg.CodeMirror.rmClass(e.getWrapperElement(), "cm-fat-cursor")
        leaveVimMode(e)
      }

      val editorId = s"$divId-ta"
      val ta = textarea(id := editorId).render.asInstanceOf[HTMLTextAreaElement]
      val editorDiv = div(id := divId, ta).render
      val params = EditorConfig.mode(editorMode)
          .theme("solarized")
          .extraKeys(scalajs.js.Dynamic.literal(
              "F6" → {(e: cm.Editor) ⇒ attach(e) },
              "F7" → {(e: cm.Editor) ⇒ detach(e) }
          ))
      val editor = CodeMirror.fromTextArea(ta, params)
      f(editor)

      DivType.Editor(editorDiv, editor)
    }

    def mkResultDiv = {
      DivType.Result(div(id := divId).render)
    }

    buf.tpe match {
      case BufferType.Editor(mode)      ⇒ mkEditorDiv(mode)
      case BufferType.Result(editorRef) ⇒ mkResultDiv
    }
  }

}

case class AEditor(editorDiv: dom.html.Div, resultDiv: dom.html.Div, editor: cm.Editor)

object DivType {
  sealed trait DivType {
    def div: dom.html.Div
  }
  case class Editor(override val div: dom.html.Div, editor: cm.Editor) extends DivType
  case class Result(override val div: dom.html.Div) extends DivType
}
