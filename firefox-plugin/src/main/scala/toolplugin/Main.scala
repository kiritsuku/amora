package toolplugin

import scala.scalajs.js.Dynamic.{ global ⇒ jsg }
import org.scalajs.dom
import org.scalajs.jquery.jQuery
import scala.scalajs.js.JSApp

object Main extends JSApp {

  private val $ = jQuery

  override def main(): Unit = {
    import scalatags.JsDom.all._

    val par = h1("Page matches ruleset").render
    $("body").replaceAll(par)
  }
}
