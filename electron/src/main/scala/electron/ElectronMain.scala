package electron

import scala.scalajs.js.Dynamic.{ global ⇒ jsg }
import scala.scalajs.js.JSApp

import io.atom.electron._

object ElectronMain extends JSApp {

  override def main(): Unit = {
    // Module to control application life
    val app = jsg.require("app").asInstanceOf[App]

    // Keep a global reference of the window object. If we don't, the window will
    // be closed automatically when the JavaScript object is GCed.
    var w: BrowserWindow = null

    app.on("ready", () ⇒ {
      w = BrowserWindow(width = 1000, height = 800)
      w.maximize()
      // TODO don't hardcode URL and port here
      w.loadURL("http://amora.center")
      w.openDevTools()
      w.on("closed", () ⇒ {
        w = null
      })

      ()
    })
  }
}
