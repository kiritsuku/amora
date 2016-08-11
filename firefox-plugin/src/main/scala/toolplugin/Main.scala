package toolplugin

import scala.scalajs.js
import scala.scalajs.js.JSApp

import org.scalajs.dom
import org.scalajs.dom.raw.Element
import org.scalajs.jquery.jQuery

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

        import org.scalajs.dom.ext._
        val h = dom.document.getElementsByClassName("highlight").head
        val tbody = h.asInstanceOf[Element].children.head
        val lines = tbody.children.zipWithIndex map { case (e, i) ⇒
          val code = e.getElementsByClassName("blob-code").head
          val text = code.textContent
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
