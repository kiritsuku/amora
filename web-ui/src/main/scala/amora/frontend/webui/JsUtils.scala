package amora.frontend.webui

import scala.scalajs.js
import scala.scalajs.js.Dynamic

import org.scalajs.dom
import org.scalajs.jquery.jQuery

trait JsUtils {
  def $ = jQuery
  def log = dom.console
  def jsg = Dynamic.global

  def f2[R](f: (js.Any, js.Any) ⇒ R): js.Function2[js.Any, js.Any, R] =
    js.Any.fromFunction2((arg0: js.Any, arg1: js.Any) ⇒ f(arg0, arg1))
}
