package amora.ui

import amora.protocol.BufferRef

class BufferManager {

  private var buffers = Map[BufferRef, Buffer]()

  private var curBuffer: Buffer = _

  def currentBuffer: Buffer =
    if (curBuffer == null) mkEditorBuf("text") else curBuffer

  def mkEditorBuf(mode: String): Buffer = {
    ???
  }

  def mkResultBuf(editorRef: BufferRef): Buffer = {
    ???
  }

  def bufferOf(bufferRef: BufferRef): Buffer = {
    buffers.get(bufferRef) match {
      case Some(buf) ⇒
        buf
      case None ⇒
        addBuf(Buffer(bufferRef, BufferType.Editor("text")))
    }
  }

  def resultBufOf(editorRef: BufferRef): Buffer = {
    val res = buffers find {
      case (_, buf) => buf.tpe match {
        case BufferType.Result(_editorRef) => _editorRef == editorRef
        case _ => false
      }
    }
    res map (_._2) getOrElse ???
  }

  private def addBuf(buf: Buffer): Buffer = {
    require(!buffers.contains(buf.ref), s"buffer ref '${buf.ref}' can't be created because it already exists")

    buffers += buf.ref → buf
    buf
  }
}
