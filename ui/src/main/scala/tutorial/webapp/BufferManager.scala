package tutorial.webapp

import shared.test.BufferRef

class BufferManager {

  private var buffers = Map[BufferRef, Buffer]()

  private var curBuffer: Buffer = _

  def currentBuffer: Buffer =
    if (curBuffer == null) mkEditorBuf("text") else curBuffer

  def mkEditorBuf(mode: String): Buffer = {
    val buf = addBuf(Buffer(mkBufRef, BufferType.Editor(mode)))
    curBuffer = buf
    buf
  }

  def mkResultBuf(editorRef: BufferRef): Buffer = {
    val buf = Buffer(mkBufRef, BufferType.Result(editorRef))
    addBuf(buf)
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

  private def mkBufRef: BufferRef =
    BufferRef(s"buf${buffers.size}")
}
