package amora.frontend

import scala.scalajs.js

package object webui extends JsUtils {

  implicit class AsDynamic[A](private val a: A) extends AnyVal {
    def jsg: js.Dynamic = a.asInstanceOf[js.Dynamic]
  }

}
