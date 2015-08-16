package shared.test

sealed trait Request
case class Interpret(id: String, code: String) extends Request
case class Execute(msg: String) extends Request

sealed trait Response
case class ConnectionSuccessful(name: String) extends Response
case object ConnectionFailure extends Response
case class Person(name: String, age: Int) extends Response
case class PersonList(persons: Seq[Person]) extends Response
case class InterpretedResult(id: String, res: String) extends Response
