package plugin

import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter

import org.junit.ComparisonFailure
import org.junit.Test

class ScalacConverterTest {

  import TestUtils._

  def convert(src: String) = {
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
    val res = g ask { () ⇒ new ScalacConverter[g.type](g).convert(tree) }

    res match {
      case util.Success(res) ⇒
        res.filterNot(Set("scala", "scala.AnyRef", ""))
      case util.Failure(f) ⇒
        throw f
    }
  }

  @Test
  def single_class() = {
    convert("package pkg; class X") === Set("pkg", "pkg.X")
  }

  @Test
  def single_object() = {
    convert("package pkg; object X") === Set("pkg", "pkg.X")
  }

  @Test
  def single_trait() = {
    convert("package pkg; trait X") === Set("pkg", "pkg.X")
  }

  @Test
  def single_abstract_class() = {
    convert("package pkg; abstract class X") === Set("pkg", "pkg.X")
  }

  @Test
  def single_def() = {
    convert("""
      package pkg
      class X {
        def a = 0
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "scala.Int")
  }

  @Test
  def single_val() = {
    convert("""
      package pkg
      class X {
        val a = 0
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "scala.Int")
  }

  @Test
  def single_lazy_val() = {
    convert("""
      package pkg
      class X {
        lazy val a = 0
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "scala.Int")
  }

  @Test
  def single_var() = {
    convert("""
      package pkg
      class X {
        var a = 0
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "pkg.X.a_=", "scala.Int")
  }

  @Test
  def getter_and_setter() = {
    convert("""
      package pkg
      class X {
        def a = 0
        def a_=(a: Int) = ()
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "pkg.X.a_=", "pkg.X.a_=.a", "scala.Int", "scala.Unit")
  }

  @Test
  def names_with_special_characters() = {
    convert("""
      package pkg
      class X_? {
        val !!! = 0
        def ??? = 0
      }
    """) === Set("pkg", "pkg.X_?", "pkg.X_?.!!!", "pkg.X_?.???", "scala.Int")
  }

  @Test
  def backticks() = {
    convert("""
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
    convert("""
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
    convert("""
      package pkg
      trait X {
        object Y
        class Z
      }
    """) === Set("pkg", "pkg.X", "pkg.X.Y", "pkg.X.Z")
  }

  @Test
  def nested_classes() = {
    convert("""
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
    convert("""
      package pkg
      class X {
        toString
      }
    """) === Set("pkg", "pkg.X", "java.lang.Object.toString")
  }

  @Test
  def chained_ref() = {
    convert("""
      package pkg
      class X {
        toString.toString.toString.toString
      }
    """) === Set("pkg", "pkg.X", "java.lang.Object.toString", "java.lang.String.toString")
  }

  @Test
  def nested_package() = {
    convert("""
      package a.b.c.d
      class X
    """) === Set("a.b.c.d", "a.b.c.d.X")
  }

  @Test
  def declaration_in_nested_package() = {
    convert("""
      package a.b.c.d
      class X {
        def x = 0
      }
    """) === Set("a.b.c.d", "a.b.c.d.X", "a.b.c.d.X.x", "scala.Int")
  }

  @Test
  def empty_package() = {
    convert("""
      class X {
        def x = 0
      }
    """) === Set("X", "X.x", "scala.Int")
  }

  @Test
  def single_import() = {
    convert("""
      import scala.collection.mutable.ListBuffer
      class X {
        ListBuffer
      }
    """) === Set("X", "scala.collection.mutable.ListBuffer")
  }

  @Test
  def multiple_imports() = {
    convert("""
      import java.io.File
      import scala.collection.mutable.Buffer
      import scala.collection.mutable.ListBuffer
      class X
    """) === Set("X", "scala.collection.mutable.Buffer", "scala.collection.mutable.ListBuffer", "java.io.File")
  }

  @Test
  def type_parameter_at_classes() = {
    convert("""
      class X[A, B]
    """) === Set("X", "X.A", "X.B")
  }

  @Test
  def type_parameter_at_methods() = {
    convert("""
      class X {
        def f[A, B] = 0
      }
    """) === Set("X", "X.f", "X.f.A", "X.f.B", "scala.Int")
  }

  @Test
  def type_parameter_at_type_ascriptions() = {
    convert("""
      class X {
        def f: Option[Int] = null
      }
    """) === Set("X", "X.f", "scala.Option", "scala.Int")
  }

  @Test
  def apply_method_implicitly() = {
    convert("""
      class X {
        def f = Option(1)
      }
    """) === Set("X", "X.f", "scala.Option", "scala.Option.apply", "scala.Int")
  }

  @Test
  def apply_method_explicitly() = {
    convert("""
      class X {
        def f = Option.apply(1)
      }
    """) === Set("X", "X.f", "scala.Option", "scala.Option.apply", "scala.Int")
  }

  @Test
  def method_with_arguments() = {
    convert("""
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
    convert("""
      class X {
        val v = 0
        def f(i: Int) = i
        f(v)
        f(Int.MinValue)
      }
    """) === Set("X", "X.v", "X.f", "X.f.i", "scala.Int", "scala.Int.MinValue")
  }

  @Test
  def empty_class_body() = {
    convert("""
      class C {}
    """) === Set("C")
  }

  @Test
  def wildcard_import() = {
    convert("""
      import scala.util._
      class X
    """) === Set("X", "scala.util")
  }

  @Test
  def wildcard_import_in_class_body() = {
    convert("""
      class X {
        import scala.util._
      }
    """) === Set("X", "scala.util")
  }

  @Test
  def new_object() = {
    convert("""
      class X {
        val x = new Object
      }
    """) === Set("X", "X.x", "java.lang.Object")
  }

  @Test
  def method_call_in_body() = {
    convert("""
      class X {
        def f = {
          toString
        }
      }
    """) === Set("X", "X.f", "java.lang.Object.toString", "java.lang.String")
  }

  @Test
  def simple_lambda() = {
    convert("""
      class X {
        def meth(f: Int ⇒ Int) = 0
      }
    """) === Set("X", "X.meth", "X.meth.f", "scala.Function1", "scala.Int")
  }

  @Test
  def call_by_name_param() = {
    convert("""
      class X {
        def meth(f: ⇒ Int) = 0
      }
    """) === Set("X", "X.meth", "X.meth.f", "scala.Function0", "scala.Int")
  }

  @Test
  def create_simple_lambda() = {
    convert("""
      class X {
        def meth(f: Int ⇒ Int) = 0
        meth(v ⇒ v)
      }
    """) === Set("X", "X.meth", "X.meth.f", "X.v", "scala.Function1", "scala.Int")
  }

}
