package amora.converter

import org.junit.Test

import amora.backend.IgnoreLogger
import amora.backend.services.ScalaSourceIndexer

class ScalacConverterTest extends ScalaCompilerTest {

  import amora.TestUtils._

  def convert(data: (String, String)*): Set[String] = {
    val indexer = new ScalaSourceIndexer(IgnoreLogger)
    val res = indexer.convertToHierarchy(data).flatMap(_._2)
    res.map(_.asString).toSet
  }

  @Test
  def return_in_def() = {
    convert("""
      class X {
        def f: Int = {
          val x = 0
          return x
        }
      }
    """) === Set(
        "X", "X.f()I", "X.f()I.x", "X.f()I.<ref>x", "scala.<ref>Int")
  }
}
