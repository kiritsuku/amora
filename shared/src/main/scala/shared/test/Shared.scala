package shared.test

case class BufferRef(id: String)

sealed trait Request
case class Interpret(id: String, code: String) extends Request
case class Execute(msg: String) extends Request
case class Control(bufferRef: BufferRef, start: Int, end: Int, controlSeq: String) extends Request
case class TextChange(bufferRef: BufferRef, start: Int, end: Int, text: String) extends Request

sealed trait Response
case class ConnectionSuccessful(name: String) extends Response
case object ConnectionFailure extends Response
case class Person(name: String, age: Int) extends Response
case class PersonList(persons: Seq[Person]) extends Response
case class InterpretedResult(id: String, res: String) extends Response
case class TextChangeAnswer(bufferRef: BufferRef, start: Int, end: Int, text: String) extends Response
