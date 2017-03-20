package pkg

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Test {
  // imports need special handling
  val lb1: ListBuffer[Int] = null
  val lb2: mutable.ListBuffer[Int] = null

  def main(args: Array[String]): Unit = {
    val world = Hello("World")
    println(s"Hello ${world.name}")
  }

  // Char.toString is Any.toString and therefore
  // needs special handling
  def f(c: Char) = c.toString

  // Nothing is a special case
  val nothingCase = classOf[Nothing]
}

case class Hello(name: String)
