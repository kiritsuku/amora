package plugin

import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter

import org.junit.ComparisonFailure
import org.junit.Test

class ScalacConverterTest {

  import TestUtils._

  def idents(src: String) = {
    val s = new Settings
    val r = new ConsoleReporter(s)
    val g = new Global(s, r)

    def withResponse[A](f: g.Response[A] ⇒ Unit) = {
      val r = new g.Response[A]
      f(r)
      r
    }

    val sf = g.newSourceFile(src, "<memory>")
    val tree = withResponse[g.Tree](g.askLoadedTyped(sf, keepLoaded = true, _)).get.left.get
    val idents = g ask { () ⇒ new ScalacConverter[g.type](g).findIdents(tree) }

    idents.filterNot(Set("scala"))
  }

  @Test
  def single_class() = {
    idents("package pkg; class X") === Set("pkg", "pkg.X")
  }

  @Test
  def single_object() = {
    idents("package pkg; object X") === Set("pkg", "pkg.X")
  }

  @Test
  def single_trait() = {
    idents("package pkg; trait X") === Set("pkg", "pkg.X")
  }

  @Test
  def single_abstract_class() = {
    idents("package pkg; abstract class X") === Set("pkg", "pkg.X")
  }

  @Test
  def single_def() = {
    idents("""
      package pkg
      class X {
        def a = 0
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "scala.Int")
  }

  @Test
  def single_val() = {
    idents("""
      package pkg
      class X {
        val a = 0
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "scala.Int")
  }

  @Test
  def single_lazy_val() = {
    idents("""
      package pkg
      class X {
        lazy val a = 0
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "scala.Int")
  }

  @Test
  def single_var() = {
    idents("""
      package pkg
      class X {
        var a = 0
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "pkg.X.a_=", "scala.Int")
  }

  @Test
  def getter_and_setter() = {
    idents("""
      package pkg
      class X {
        def a = 0
        def a_=(a: Int) = ()
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "pkg.X.a_=", "pkg.X.a_=.a", "scala.Int", "scala.Unit")
  }

  @Test
  def names_with_special_characters() = {
    idents("""
      package pkg
      class X_? {
        val !!! = 0
        def ??? = 0
      }
    """) === Set("pkg", "pkg.X_?", "pkg.X_?.!!!", "pkg.X_?.???", "scala.Int")
  }

  @Test
  def backticks() = {
    idents("""
      package pkg
      class `A B C` {
        val _ = 0
        val a_b_c = 0
        val `a b c` = 0
        def `d e f` = 0
        def `type` = 0
      }
    """) === Set("pkg", "scala.Int", "pkg.`A B C`", "pkg.`A B C`._", "pkg.`A B C`.a_b_c", "pkg.`A B C`.`a b c`", "pkg.`A B C`.`d e f`", "pkg.`A B C`.`type`")
  }
}
