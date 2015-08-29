package ui

import protocol._

case class Buffer(ref: BufferRef, tpe: BufferType.BufferType) {
  var mode: String = Mode.Normal
}

object BufferType {
  sealed trait BufferType
  case class Editor(mode: String) extends BufferType
  case class Result(correspondingEditor: BufferRef) extends BufferType
}
