package shared.test

object Shared {
  def func(i: Int) = i*3
}

trait Message
case class Person(name: String, age: Int) extends Message
