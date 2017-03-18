package pkg

object Test {
  def main(args: Array[String]): Unit = {
    val world = Hello("World")
    println(s"Hello ${world.name}")
  }
}

case class Hello(name: String)
