package pkg

object Test {
  def main(args: Array[String]): Unit = {
    val world = Hello("World")
    println(s"Hello $world")
  }
}

case class Hello(name: String)
