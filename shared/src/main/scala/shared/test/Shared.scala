package shared.test

object Shared {
  def func(i: Int) = i*3
}

sealed trait Request
case class Interpret(code: String) extends Request
case class Execute(msg: String) extends Request

sealed trait Response
case class Person(name: String, age: Int) extends Response
