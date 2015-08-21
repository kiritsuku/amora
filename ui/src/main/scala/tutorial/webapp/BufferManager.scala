package tutorial.webapp

import shared.test.BufferRef

class BufferManager {

  private var buffers = Map[BufferRef, Buffer]()

  def mkEditorBuf(mode: String): Buffer = {
    val buf = Buffer(mkBufRef, BufferType.Editor(mode))
    addBuf(buf)
  }

  def mkResultBuf(editorRef: BufferRef): Buffer = {
    val buf = Buffer(mkBufRef, BufferType.Result(editorRef))
    addBuf(buf)
  }

  def resultBufOf(editorRef: BufferRef): Buffer = {
    println(s">> $editorRef")
    val res = buffers find {
      case (_, buf) => println(">" + buf.tpe); buf.tpe match {
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
