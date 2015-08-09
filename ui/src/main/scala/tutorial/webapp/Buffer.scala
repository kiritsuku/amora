package tutorial.webapp

case class Buffer(ref: BufferRef, tpe: BufferType.BufferType)

case class BufferRef(id: String)

object BufferType {
  sealed trait BufferType
  case class Editor(mode: String) extends BufferType
  case class Result(correspondingEditor: BufferRef) extends BufferType
}
