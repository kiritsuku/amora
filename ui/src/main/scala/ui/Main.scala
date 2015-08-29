package ui

import scala.scalajs.js.JSApp

object Main extends JSApp {

  val ui = new Ui

  override def main(): Unit = {
    ui.authenticate()
    ui.setupUI2()
  }

}
