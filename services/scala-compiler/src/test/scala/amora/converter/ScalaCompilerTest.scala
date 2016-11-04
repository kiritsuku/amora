package amora.converter

import org.junit.Test

abstract class ScalaCompilerTest {

  import amora.TestUtils._

  def convert(src: String): Set[String] =
    convert("<memory>" → src)

  def convert(data: (String, String)*): Set[String]

  @Test
  def single_package() = {
    convert("""
      package pkg
    """) === Set(
        "pkg")
  }

  @Test
  def single_class() = {
    convert("""
      package pkg
      class X
    """) === Set(
        "pkg", "pkg.X", "scala.<ref>AnyRef", "pkg.X.this()V")
  }
}
