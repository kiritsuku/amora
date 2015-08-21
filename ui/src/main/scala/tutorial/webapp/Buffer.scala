package tutorial.webapp

import shared.test.BufferRef

case class Buffer(ref: BufferRef, tpe: BufferType.BufferType)

object BufferType {
  sealed trait BufferType
  case class Editor(mode: String) extends BufferType
  case class Result(correspondingEditor: BufferRef) extends BufferType
}
