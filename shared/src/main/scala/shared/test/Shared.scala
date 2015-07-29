package shared.test

sealed trait Request
case class Interpret(code: String) extends Request
case class Execute(msg: String) extends Request

sealed trait Response
case class Person(name: String, age: Int) extends Response
