package toolplugin

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{ global ⇒ jsg }
import org.scalajs.dom
import org.scalajs.jquery.jQuery
import scala.scalajs.js.JSApp
import org.scalajs.dom.raw.Element

object Main extends JSApp {

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }
  implicit class AsDynamic[A](private val a: A) extends AnyVal {
    def jsg: js.Dynamic = a.asInstanceOf[js.Dynamic]
  }

  private val $ = jQuery

  override def main(): Unit = {
    import scalatags.JsDom.all._

    dom.window.location.pathname match {
      // for directories
      // case r"""/([^/]+)$username/([^/]+)$reponame/tree/([^/]+)$branch/(.+)$filename""" ⇒

      // for files
      case r"""/([^/]+)$username/([^/]+)$reponame/blob/([^/]+)$branch/(.+)$filename""" ⇒
        println(s"username: $username")
        println(s"reponame: $reponame")
        println(s"branch: $branch")
        println(s"filename: $filename")

        val h = dom.document.getElementsByClassName("highlight")(0)
        val tbody = h.asInstanceOf[Element].children(0)
        val trs = tbody.children
        val lines = for (i ← 0 until trs.length) yield {
          val code = trs(i).getElementsByClassName("blob-code")(0)
          val text = code.jsg.innerText
          (i+1) → text
        }
        lines foreach println

      case path ⇒
        println(s"path `$path` didn't match pattern")
    }
    val par = h1(s"Page matches ruleset").render
    $("body").prepend(par)

  }
}
