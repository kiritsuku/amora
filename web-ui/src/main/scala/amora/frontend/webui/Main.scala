package amora.frontend.webui

import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom
import org.scalajs.dom.raw.Event
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.MouseEvent

import amora.frontend.webui.protocol._

@JSExport
object Main extends JSApp with Requests {

  private val connection = new Connection(handleResponse)

  /**
   * If a new web page is created, this is `false`. Once some content has been
   * loaded, this needs to be set to `true` in order to signal that new state
   * shall not override already existing state but update it.
   */
  private var hasState = false

  override def main(): Unit = {
    connection.setup()
  }

  def handleResponse(response: Response): Unit = response match {
    case ConnectionSuccessful ⇒
      dom.console.info(s"Connection to server established. Communication is now possible.")
      if (!hasState) {
        hasState = true
        mkMainPage()
      }

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

  def mkMainPage() = {
    import scalatags.JsDom.all._

    val content = div(
        h3("Knowledge Base"),
        ul(
          li(id := "li1", a(href := "", "Show queue", onclick := "return false;")),
          li(id := "li2", a(href := "", "Show schemas", onclick := "return false;"))
        ),
        div(id := "content"),
        div(pre(code(id := "editor", """
          |class X {
          |  val xs: List[Int] = List(1)
          |  val ys: List[Int] = xs
          |}
          |class Y {
          |  val x = new X
          |  def f() = {
          |    val xs = x.xs
          |    xs
          |  }
          |  import x._
          |  val zs = xs
          |}
        """.stripMargin.trim()))),
        button(id := "editorButton", `type` := "button", "index code")
    ).render
    $("body").append(content)

    handleClickEvent("li1")(_ ⇒ connection.send(GetQueueItems))
    handleClickEvent("li2")(_ ⇒ connection.send(GetSchemas))
    handleClickEvent("editorButton") { _ ⇒
      val text = $("#editor").text()
      onSuccess(indexScalaSrc(text)) { _ ⇒
        log.log("Data indexed")
      }
    }
    val d = htmlElem("editor")
    d.onmouseup = (e: MouseEvent) ⇒ {
      val sel = selection("editor")

      onSuccess(findDeclaration(sel.start)) { range ⇒
        log.info("findDeclaration: " + range)
      }
      onSuccess(findUsages(sel.start)) { ranges ⇒
        val text = $("#editor").text()
        val sb = new StringBuilder
        var begin = 0
        for (Range(start, end) ← ranges) {
          sb.append(text.substring(begin, start))
          sb.append("""<span style="background-color: #84C202">""")
          sb.append(text.substring(start, end))
          sb.append("</span>")
          begin = end
        }
        sb.append(text.substring(begin, text.length))
        $("#editor").html(sb.toString())
      }
    }

    mkExplorer()
  }

  def mkExplorer() = {
    import scalatags.JsDom.all._

    $("body").append(div(
        h3("Explorer"),
        div(id := "explorer-content", div()),
        button(id := "explorer-button", `type` := "button", "Get Root")
    ).render)

    def mkId(entry: HierarchyEntry) =
      "entry" + entry.hashCode().toString()

    var openedEntries = Set[String]()
    var allEntries = Map[String, HierarchyEntry]()

    def add(entryId: String, entry: Hierarchy): Unit = {
      val content = $(s"#$entryId > div")
      onSuccess(findChildren(entry)) { entries ⇒
        content.empty()
        content.append(ul(
            if (entries.isEmpty)
              li("<none>")
            else
              for (e ← entries) yield li(id := mkId(e), a(href := e.url, e.name), div())
        ).render)

        for (e ← entries) {
          val entryId = mkId(e)
          allEntries += entryId → e
        }
      }
    }

    handleClickEvent("explorer-content") { event ⇒
      val elem = dom.document.elementFromPoint(event.clientX, event.clientY)
      val entryId = elem.id

      if (entryId != null && entryId.nonEmpty) {
        val e = allEntries(entryId)
        val isOpen = openedEntries(entryId)
        if (isOpen) {
          openedEntries -= entryId
          $(s"#$entryId > div").empty()
        }
        else {
          openedEntries += entryId
          add(entryId, e)
        }
      }
    }

    handleClickEvent("explorer-button") { _ ⇒
      openedEntries = Set()
      allEntries = Map()
      add("explorer-content", Root)
    }
  }

  def htmlElem(id: String): HTMLElement =
    dom.document.getElementById(id).asInstanceOf[HTMLElement]

  /**
   * Returns the selection that belongs to a HTML element of a given `id` as an
   * instance of [[Range]], whose `start` and `end` are relative to the
   * beginning of the HTML element.
   */
  def selection(id: String): Range = {
    val range = dom.window.getSelection().getRangeAt(0)
    val content = range.cloneRange()
    content.selectNodeContents(htmlElem(id))
    content.setEnd(range.startContainer, range.startOffset)
    val start = content.toString().length()

    content.setStart(range.startContainer, range.startOffset)
    content.setEnd(range.endContainer, range.endOffset)
    val len = content.toString().length()
    Range(start, start+len)
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

    for (i ← items.items) handleClickEvent(s"item$i")(_ ⇒ connection.send(GetQueueItem(i)))
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
      connection.send(GetSchema(selectedSchema))
    }
  }

  // TODO Get rid of this @JSExport
  // It is a hack which was needed to call the Scala code from Alpaca.js
  @JSExport
  def handleFormSubmit(elem: js.Object) = {
    val value = elem.jsg.getValue()
    val formattedJson = JSON.stringify(value, null: js.Function2[String, js.Any, js.Any], "  ")
    connection.send(IndexData(formattedJson))
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

  private def handleClickEvent(id: String)(f: MouseEvent ⇒ Unit) = {
    val d = htmlElem(id)
    d.onclick = (e: MouseEvent) ⇒ f(e)
  }
}
