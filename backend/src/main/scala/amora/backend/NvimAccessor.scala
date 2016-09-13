package amora.backend

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import akka.actor.ActorRef
import akka.actor.ActorSystem
import amora.backend.actors.NvimMsg.NvimSignal
import amora.backend.internal.WindowTreeCreator
import nvim.{Selection ⇒ _, _}
import nvim.internal.Notification
import amora.protocol.{Mode ⇒ _, _}
import amora.protocol.ui.WindowTree

final class NvimAccessor(self: ActorRef)(implicit system: ActorSystem) {
  import system.dispatcher

  private val nvim = Nvim(Connection("127.0.0.1", 6666))

  private var windows = Set[Int]()

  /**
   * We need to track the active window here in order to avoid to tell nvim to update
   * the active window to the very same window that is already active. In case we do
   * this, nvim gets confused and no longer can no longer recognize events that
   * belong together.
   */
  private var activeWinId = -1

  /**
   * Whenever this is set to `true`, the next WinEnter event that is received by
   * `handler` needs to be ignored. This is necessary because in some cases we
   * know that we changed the active window and therefore there may not be a
   * need to handle the sent WinEnter event.
   */
  @volatile
  private var ignoreNextWinEnter = false

  object events {
    val WinEnter = "_WinEnter"
    val WinLeave = "_WinLeave"

    val AllEvents = Seq(WinEnter, WinLeave)
  }

  private val handler: Notification ⇒ Unit = n ⇒ {
    import events._
    n.method match {
      case WinEnter if ignoreNextWinEnter ⇒
        ignoreNextWinEnter = false
      case WinEnter ⇒
        val resp = updateWindows() flatMap (_ ⇒ clientUpdate)

        handle(resp, "Failed to send a broadcast event.") { resp ⇒
          NvimSignal(resp)
        }

      case WinLeave ⇒

      case _ ⇒
        system.log.warning(s"Notification for unknown event type arrived: $n")
    }
  }

  nvim.connection.addNotificationHandler(handler)
  events.AllEvents foreach nvim.subscribe
  updateWindows()

  private def updateWindows(): Future[Unit] = {
    nvim.windows map { ws ⇒
      val winIds = ws.map(_.id).toSet
      val removed = windows diff winIds
      val added = winIds diff windows
      windows --= removed
      windows ++= added
    }
  }

  private def bufferContent(b: Buffer): Future[Seq[String]] = for {
    count ← b.lineCount
    s ← b.lineSlice(0, count)
  } yield s

  private def selection = for {
    win ← nvim.window
    buf ← win.buffer
    sel ← nvim.selection
  } yield {
    val List(start, end) = List(
      Pos(sel.start.row-1, sel.start.col-1),
      Pos(sel.end.row-1, sel.end.col-1)
    ).sorted
    Selection(win.id, buf.id, start, end)
  }

  private def winOf(winId: Int): Future[Window] =
    Future.successful(Window(winId, nvim.connection))

  private def winInfo(winId: Int) = for {
    win ← winOf(winId)
    buf ← win.buffer
    content ← bufferContent(buf)
    pos ← win.position
    w ← win.width
    h ← win.height
  } yield WindowUpdate(win.id, buf.id, content, WinDim(pos.row, pos.col, w, h))

  private def clientUpdate = for {
    wins ← Future.sequence(windows map winInfo)
    mode ← nvim.activeMode
    sel ← selection
    tree ← windowTree
  } yield ClientUpdate(wins.toSeq, Mode.asString(mode), sel, Some(tree))

  private def windowTree: Future[WindowTree] = for {
    windows ← Future.sequence(windows map winOf)
    infos ← Future.sequence(windows.toList map { win ⇒
      for {
        pos ← win.position
        w ← win.width
        h ← win.height
      } yield WindowTreeCreator.WinInfo(win.id, pos.col, pos.row, w, h)
    })
  } yield WindowTreeCreator.mkWindowTree(infos)

  def handleClientJoined(sender: String): Unit = {
    val resp = clientUpdate

    handle(resp, s"Failed to send an update to the client `$sender`.") {
      resp ⇒ NvimSignal(sender, resp)
    }
  }

  def handleTextChange(change: TextChange, sender: String): Unit = {
    system.log.info(s"received: $change")
    val resp = for {
      _ ← updateActiveWindow(change.winId)
      _ ← nvim.sendInput(change.text)
      update ← winInfo(change.winId)
      mode ← nvim.activeMode
      s ← selection
    } yield ClientUpdate(Seq(update), Mode.asString(mode), s, None)

    handle(resp, s"Failed to send response after client request `$change`.") {
      resp ⇒ NvimSignal(sender, resp)
    }
  }

  def handleSelectionChange(change: SelectionChange, sender: String): Unit = {
    system.log.info(s"received: $change")
    val resp = for {
      w ← nvim.window
      _ = if (w.id != change.winId) ignoreNextWinEnter = true
      win ← updateActiveWindow(change.winId)
      _ ← win.cursor = Position(change.cursorRow+1, change.cursorColumn)
      s ← selection
    } yield SelectionChangeAnswer(win.id, change.bufferId, s)

    handle(resp, s"Failed to send response after client request `$change`.") {
      resp ⇒ NvimSignal(sender, resp)
    }
  }

  def handleControl(control: Control, sender: String): Unit = {
    system.log.info(s"received: $control")
    val resp = for {
      _ ← updateActiveWindow(control.winId)
      _ ← nvim.sendInput(control.controlSeq)
      update ← winInfo(control.winId)
      mode ← nvim.activeMode
      s ← selection
    } yield ClientUpdate(Seq(update), Mode.asString(mode), s, None)

    handle(resp, s"Failed to send response after client request `$control`.") {
      resp ⇒ NvimSignal(sender, resp)
    }
  }

  private def updateActiveWindow(winId: Int): Future[Window] =
    if (activeWinId == winId)
      winOf(activeWinId)
    else {
      activeWinId = winId
      nvim.window = winId
    }

  private def handle[A, B](f: Future[A], errMsg: String)(onSuccess: A ⇒ B): Unit = {
    f onComplete {
      case Success(a) ⇒
        val res = onSuccess(a)
        self ! res
        system.log.info(s"sent: $res")

      case Failure(t) ⇒
        system.log.error(t, errMsg)
    }
  }
}
