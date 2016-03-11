package plugin

import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter

import org.junit.ComparisonFailure
import org.junit.Test

class IdentFinderTest {

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

    idents.filterNot(Set("scala")).toList
  }

  @Test
  def single_class() = {
    idents("package pkg; class X") === List("pkg", "pkg.X")
  }

  @Test
  def single_object() = {
    idents("package pkg; object X") === List("pkg", "pkg.X")
  }

  @Test
  def single_trait() = {
    idents("package pkg; trait X") === List("pkg", "pkg.X")
  }

  @Test
  def single_abstract_class() = {
    idents("package pkg; abstract class X") === List("pkg", "pkg.X")
  }
}
