package pkg

import scala.collection.mutable.ListBuffer

object Test {
  val lb: ListBuffer[Int] = null
  def main(args: Array[String]): Unit = {
    val world = Hello("World")
    println(s"Hello ${world.name}")
  }

  // Nothing is a special case
  val nothingCase = classOf[Nothing]
}

case class Hello(name: String)
