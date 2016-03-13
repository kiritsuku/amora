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

    idents.filterNot(Set("scala", "scala.AnyRef", ""))
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

  @Test
  def nested_members() = {
    idents("""
      package pkg
      class X {
        def a = {
          def b = {
            val c = {
              val d = 0
              d
            }
            c
          }
          b
        }
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "pkg.X.a.b", "pkg.X.a.b.c", "pkg.X.a.b.c.d", "scala.Int")
  }

  @Test
  def nested_in_trait() = {
    idents("""
      package pkg
      trait X {
        object Y
        class Z
      }
    """) === Set("pkg", "pkg.X", "pkg.X.Y", "pkg.X.Z")
  }

  @Test
  def nested_classes() = {
    idents("""
      package pkg
      class X {
        trait Y {
          object Z {
            val a = 0
            def b = 0
          }
        }
      }
    """) === Set("pkg", "pkg.X", "pkg.X.Y", "pkg.X.Y.Z", "pkg.X.Y.Z.a", "pkg.X.Y.Z.b", "scala.Int")
  }

  @Test
  def simple_ref() = {
    idents("""
      package pkg
      class X {
        toString
      }
    """) === Set("pkg", "pkg.X", "pkg.X.toString", "java.lang.Object.toString")
  }

  @Test
  def chained_ref() = {
    idents("""
      package pkg
      class X {
        toString.toString.toString.toString
      }
    """) === Set("pkg", "pkg.X", "pkg.X.toString", "pkg.X.toString.toString", "pkg.X.toString.toString.toString", "pkg.X.toString.toString.toString.toString", "java.lang.Object.toString", "java.lang.String.toString")
  }

  @Test
  def nested_package() = {
    idents("""
      package a.b.c.d
      class X
    """) === Set("a.b.c.d", "a.b.c.d.X")
  }

  @Test
  def declaration_in_nested_package() = {
    idents("""
      package a.b.c.d
      class X {
        def x = 0
      }
    """) === Set("a.b.c.d", "a.b.c.d.X", "a.b.c.d.X.x", "scala.Int")
  }

  @Test
  def empty_package() = {
    idents("""
      class X {
        def x = 0
      }
    """) === Set("X", "X.x", "scala.Int")
  }

  @Test
  def single_import() = {
    idents("""
      import scala.collection.mutable.ListBuffer
      class X {
        ListBuffer
      }
    """) === Set("X", "scala.collection.mutable.ListBuffer")
  }

  @Test
  def multiple_imports() = {
    idents("""
      import java.io.File
      import scala.collection.mutable.Buffer
      import scala.collection.mutable.ListBuffer
      class X
    """) === Set("X", "scala.collection.mutable.Buffer", "scala.collection.mutable.ListBuffer", "java.io.File")
  }

  @Test
  def type_parameter_at_classes() = {
    idents("""
      class X[A, B]
    """) === Set("X", "X.A", "X.B")
  }

  @Test
  def type_parameter_at_methods() = {
    idents("""
      class X {
        def f[A, B] = 0
      }
    """) === Set("X", "X.f", "X.f.A", "X.f.B", "scala.Int")
  }

  @Test
  def type_parameter_at_type_ascriptions() = {
    idents("""
      class X {
        def f: Option[Int] = null
      }
    """) === Set("X", "X.f", "scala.Option", "scala.Int")
  }

  @Test
  def apply_method_implicitly() = {
    idents("""
      class X {
        def f = Option(1)
      }
    """) === Set("X", "X.f", "scala.Option", "scala.Option.apply", "scala.Int")
  }

  @Test
  def apply_method_explicitly() = {
    idents("""
      class X {
        def f = Option.apply(1)
      }
    """) === Set("X", "X.f", "scala.Option", "scala.Option.apply", "scala.Int")
  }

  @Test
  def method_with_arguments() = {
    idents("""
      class X {
        def f(i: Int, s: String) = {
          def g(i: Int) = 0
          0
        }
      }
    """) === Set("X", "X.f", "X.f.i", "X.f.s", "X.f.g", "X.f.g.i", "scala.Int", "java.lang.String")
  }

  @Test
  def call_method_with_arguments() = {
    idents("""
      class X {
        val v = 0
        def f(i: Int) = i
        f(v)
        f(Int.MinValue)
      }
    """) === Set("X", "X.v", "X.f", "X.f.i", "scala.Int", "scala.Int.MinValue")
  }
}
