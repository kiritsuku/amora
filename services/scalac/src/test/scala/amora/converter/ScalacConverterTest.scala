package amora.converter

import org.junit.Test

import amora.backend.IgnoreLogger
import amora.backend.services.ScalaSourceIndexer
import amora.converter.protocol._

class ScalacConverterTest extends ScalaCompilerTest {

  import amora.TestUtils._

  def convert(data: (String, String)*): Set[String] = {
    val PkgFinder = """(?s).*?package ([\w\.]+).*?""".r
    val artifact = Artifact(Project("p"), "o", "n", "v1")
    def mkPkg(pkgs: Seq[String]): Schema = pkgs match {
      case Nil ⇒ artifact
      case pkg +: pkgs ⇒ Package(pkg, mkPkg(pkgs))
    }

    val res = new ScalaSourceIndexer(IgnoreLogger).convertToHierarchy(data map {
      case (fileName, src) ⇒
        val file = src match {
          case PkgFinder(name) ⇒ File(mkPkg(name.split('.').reverse), fileName)
          case _ ⇒ File(artifact, fileName)
        }
        (file, src)
    }).flatMap(_._2)
    res.map(_.asString).toSet
  }

  @Test
  def single_object() = {
    convert("""
      package pkg
      object X
    """) === Set(
        "pkg", "pkg.<object>X", "scala.<ref>AnyRef", "pkg.<object>X.this()V")
  }

  @Test
  def single_trait() = {
    convert("""
      package pkg
      trait X
    """) === Set(
        "pkg", "pkg.X", "scala.<ref>AnyRef")
  }

  @Test
  def single_abstract_class() = {
    convert("""
      package pkg
      abstract class X
    """) === Set(
        "pkg", "pkg.X", "scala.<ref>AnyRef", "pkg.X.this()V")
  }

  @Test
  def single_def() = {
    convert("""
      package pkg
      class X {
        def a = 0
      }
    """) === Set(
        "pkg", "pkg.X", "pkg.X.a()I", "scala.<ref>Int", "scala.<ref>AnyRef",
        "pkg.X.this()V")
  }

  @Test
  def single_val() = {
    convert("""
      package pkg
      class X {
        val a = 0
      }
    """) === Set(
        "pkg", "pkg.X", "pkg.X.a", "scala.<ref>Int", "scala.<ref>AnyRef",
        "pkg.X.this()V")
  }

  @Test
  def single_lazy_val() = {
    convert("""
      package pkg
      class X {
        lazy val a = 0
      }
    """) === Set(
        "pkg", "pkg.X", "pkg.X.a", "scala.<ref>Int", "scala.<ref>AnyRef",
        "pkg.X.this()V")
  }

  @Test
  def single_var() = {
    convert("""
      package pkg
      class X {
        var a = 0
      }
    """) === Set(
        "pkg", "pkg.X", "pkg.X.a", "scala.<ref>Int", "scala.<ref>AnyRef",
        "pkg.X.this()V")
  }

  @Test
  def getter_and_setter() = {
    convert("""
      package pkg
      class X {
        def a = 0
        def a_=(a: Int) = ()
      }
    """) === Set(
        "pkg", "pkg.X", "pkg.X.a()I", "pkg.X.a_=(I)V", "pkg.X.a_=(I)V.<param>a",
        "scala.<ref>Int", "scala.<ref>Unit", "scala.<ref>AnyRef", "pkg.X.this()V")
  }

  @Test
  def names_with_special_characters() = {
    convert("""
      package pkg
      class X_? {
        val !!! = 0
        def ??? = 0
      }
    """) === Set(
        "pkg", "pkg.X_?", "pkg.X_?.!!!", "pkg.X_?.???()I", "scala.<ref>Int",
        "scala.<ref>AnyRef", "pkg.X_?.this()V")
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
    """) === Set(
        "pkg", "scala.<ref>Int", "pkg.`A B C`", "pkg.`A B C`._", "pkg.`A B C`.a_b_c",
        "pkg.`A B C`.`a b c`", "pkg.`A B C`.`d e f`()I", "pkg.`A B C`.`type`()I",
        "scala.<ref>AnyRef", "pkg.`A B C`.this()V")
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
    """) === Set(
        "pkg", "pkg.X", "pkg.X.a()I", "pkg.X.a()I.b()I", "pkg.X.a()I.b()I.c",
        "pkg.X.a()I.b()I.c.d", "scala.<ref>Int", "pkg.X.a()I.<ref>b()I",
        "pkg.X.a()I.b()I.<ref>c", "pkg.X.a()I.b()I.c.<ref>d", "scala.<ref>AnyRef",
        "pkg.X.this()V")
  }

  @Test
  def nested_in_trait() = {
    convert("""
      package pkg
      trait X {
        object Y
        class Z
      }
    """) === Set(
        "pkg", "pkg.X", "pkg.X.<object>Y", "pkg.X.Z", "scala.<ref>AnyRef", "pkg.X.<object>Y.this()V", "pkg.X.Z.this()V")
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
    """) === Set(
        "pkg", "pkg.X", "pkg.X.Y", "pkg.X.Y.<object>Z", "pkg.X.Y.<object>Z.a", "pkg.X.Y.<object>Z.b()I",
        "scala.<ref>Int", "scala.<ref>AnyRef", "pkg.X.this()V", "pkg.X.Y.<object>Z.this()V")
  }

  @Test
  def simple_ref() = {
    convert("""
      package pkg
      class X {
        toString
      }
    """) === Set(
        "pkg", "pkg.X", "java.lang.Object.<ref>toString()Ljava/lang/String;",
        "scala.<ref>AnyRef", "pkg.X.this()V")
  }

  @Test
  def chained_ref() = {
    convert("""
      package pkg
      class X {
        toString.toString.toString.toString
      }
    """) === Set(
        "pkg", "pkg.X", "java.lang.Object.<ref>toString()Ljava/lang/String;",
        "java.lang.String.<ref>toString()Ljava/lang/String;", "scala.<ref>AnyRef",
        "pkg.X.this()V")
  }

  @Test
  def nested_package() = {
    convert("""
      package a.b.c.d
      class X
    """) === Set(
        "a.b.c.d", "a.b.c.d.X", "scala.<ref>AnyRef", "a.b.c.d.X.this()V")
  }

  @Test
  def declaration_in_nested_package() = {
    convert("""
      package a.b.c.d
      class X {
        def x = 0
      }
    """) === Set(
        "a.b.c.d", "a.b.c.d.X", "a.b.c.d.X.x()I", "scala.<ref>Int", "scala.<ref>AnyRef",
        "a.b.c.d.X.this()V")
  }

  @Test
  def empty_package() = {
    convert("""
      class X {
        def x = 0
      }
    """) === Set(
        "X", "X.x()I", "scala.<ref>Int", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def single_import() = {
    convert("""
      import scala.collection.mutable.ListBuffer
      class X {
        ListBuffer
      }
    """) === Set(
        "X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable",
        "scala.collection.mutable.<ref>ListBuffer", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def multiple_imports() = {
    convert("""
      import java.io.File
      import scala.collection.mutable.Buffer
      import scala.collection.mutable.ListBuffer
      class X
    """) === Set(
        "X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable",
        "scala.collection.mutable.<ref>Buffer", "scala.collection.mutable.<ref>ListBuffer",
        "<ref>java", "java.<ref>io", "java.io.<ref>File", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def type_parameter_at_classes() = {
    convert("""
      class X[A, B]
    """) === Set(
        "X", "X.<tparam>A", "X.<tparam>B", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def type_parameter_at_methods() = {
    convert("""
      class X {
        def f[A, B] = 0
      }
    """) === Set(
        "X", "X.f()I", "X.f()I.<tparam>A", "X.f()I.<tparam>B", "scala.<ref>Int",
        "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def type_parameter_at_type_ascriptions() = {
    convert("""
      class X {
        def f: Option[Int] = null
      }
    """) === Set(
        "X", "X.f()Lscala/Option;", "scala.<ref>Option", "scala.<ref>Int",
        "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def apply_method_implicitly() = {
    convert("""
      class X {
        def f = Option(1)
      }
    """) === Set(
        "X", "X.f()Lscala/Option;", "scala.<ref>Option", "scala.<ref>AnyRef",
        "scala.Option.<ref>apply(Ljava/lang/Object;)Lscala/Option;", "scala.<ref>Int",
        "X.this()V")
  }

  @Test
  def apply_method_explicitly() = {
    convert("""
      class X {
        def f = Option.apply(1)
      }
    """) === Set(
        "X", "X.f()Lscala/Option;", "scala.<ref>Option", "scala.<ref>AnyRef",
        "scala.Option.<ref>apply(Ljava/lang/Object;)Lscala/Option;", "scala.<ref>Int",
        "X.this()V")
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
    """) === Set(
        "X", "X.f(ILjava/lang/String;)I", "X.f(ILjava/lang/String;)I.<param>i",
        "X.f(ILjava/lang/String;)I.<param>s", "X.f(ILjava/lang/String;)I.g(I)I",
        "X.f(ILjava/lang/String;)I.g(I)I.<param>i", "scala.<ref>Int", "scala.Predef.<ref>String",
        "scala.<ref>AnyRef", "X.this()V")
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
    """) === Set(
        "X", "X.v", "X.f(I)I", "X.f(I)I.<param>i", "X.<ref>f(I)I", "X.f(I)I.<ref>i",
        "X.<ref>v", "scala.<ref>Int", "scala.Int.<ref>MinValue", "scala.<ref>AnyRef",
        "X.this()V")
  }

  @Test
  def empty_class_body() = {
    convert("""
      class C {}
    """) === Set(
        "C", "scala.<ref>AnyRef", "C.this()V")
  }

  @Test
  def wildcard_import() = {
    convert("""
      import scala.util._
      class X
    """) === Set(
        "X", "<ref>scala", "scala.<ref>util", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def wildcard_import_in_class_body() = {
    convert("""
      class X {
        import scala.util._
      }
    """) === Set(
        "X", "<ref>scala", "scala.<ref>util", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def new_object() = {
    convert("""
      class X {
        val x = new Object
      }
    """) === Set(
        "X", "X.x", "java.lang.<ref>Object", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def method_call_in_body() = {
    convert("""
      class X {
        def f = {
          toString
        }
      }
    """) === Set(
        "X", "X.f()Ljava/lang/String;", "java.lang.Object.<ref>toString()Ljava/lang/String;",
        "java.lang.<ref>String", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def simple_lambda() = {
    convert("""
      class X {
        def meth(f: Int ⇒ Int) = 0
      }
    """) === Set(
        "X", "X.meth(Lscala/Function1;)I", "X.meth(Lscala/Function1;)I.<param>f",
        "scala.<ref>Function1", "scala.<ref>Int", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def call_by_name_param() = {
    convert("""
      class X {
        def meth(f: ⇒ Int) = 0
      }
    """) === Set(
        "X", "X.meth(Lscala/Function0;)I", "X.meth(Lscala/Function0;)I.<param>f",
        "scala.<ref>Function0", "scala.<ref>Int", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def create_simple_lambda() = {
    convert("""
      class X {
        def meth(f: Int ⇒ Int) = 0
        meth(v ⇒ v)
      }
    """) === Set(
        "X", "X.meth(Lscala/Function1;)I", "X.meth(Lscala/Function1;)I.<param>f",
        "X.<ref>meth(Lscala/Function1;)I", "X.<ref>meth(Lscala/Function1;)I.<param>v",
        "X.<ref>meth(Lscala/Function1;)I.<ref>v", "scala.<ref>Function1", "scala.<ref>Int",
        "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def reference_across_multiple_files() = {
    convert(
    "f1.scala" → """
      package a
      import b.Y
      class X {
        def m: Y = null
      }
    """,
    "f2.scala" → """
      package b
      class Y
    """) === Set(
        "a", "a.X", "a.X.m()Lb/Y;", "b", "b.Y", "<ref>b", "b.<ref>Y",
        "scala.<ref>AnyRef", "a.X.this()V", "b.Y.this()V")
  }

  @Test
  def lambda_with_multiple_parameters() = {
    convert("""
      class X {
        def meth(f: (Int, Int, Int) ⇒ Int) = 0
        meth((a, b, c) ⇒ 0)
      }
    """) === Set(
        "X", "X.meth(Lscala/Function3;)I", "X.meth(Lscala/Function3;)I.<param>f",
        "X.<ref>meth(Lscala/Function3;)I", "X.<ref>meth(Lscala/Function3;)I.<param>a",
        "X.<ref>meth(Lscala/Function3;)I.<param>b", "X.<ref>meth(Lscala/Function3;)I.<param>c",
        "scala.<ref>Function3", "scala.<ref>Int", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def chained_lambda() = {
    convert("""
      class X {
        def meth(f: Int ⇒ Int ⇒ Int ⇒ Int) = 0
        meth(a ⇒ b ⇒ c ⇒ 0)
      }
    """) === Set(
        "X", "X.meth(Lscala/Function1;)I", "X.meth(Lscala/Function1;)I.<param>f",
        "X.<ref>meth(Lscala/Function1;)I", "X.<ref>meth(Lscala/Function1;)I.<param>a",
        "X.<ref>meth(Lscala/Function1;)I.<param>b", "X.<ref>meth(Lscala/Function1;)I.<param>c",
        "scala.<ref>Function1", "scala.<ref>Int", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def simple_inheritance_in_single_file() = {
    convert("""
      class X
      class Y extends X
    """) === Set(
        "X", "Y", "<ref>X", "scala.<ref>AnyRef", "X.this()V", "Y.this()V")
  }

  @Test
  def simple_inheritance_in_multiple_files() = {
    convert(
    "f1.scala" → """
      package a
      import b.Y
      class X extends Y
    """,
    "f2.scala" → """
      package b
      class Y
    """) === Set(
        "a", "a.X", "b", "b.Y", "<ref>b", "b.<ref>Y", "scala.<ref>AnyRef",
        "a.X.this()V", "b.Y.this()V")
  }

  @Test
  def simple_inheritance_from_stdlib_class() = {
    convert("""
      trait X extends scala.collection.mutable.AbstractSet[Int]
    """) === Set(
        "X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable",
        "scala.collection.mutable.<ref>AbstractSet", "scala.<ref>Int")
  }

  @Test
  def simple_inheritance_from_multiple_stdlib_classes() = {
    convert("""
      trait X extends collection.SeqLike[Int, Int]
        with collection.IterableLike[Int, Int]
        with collection.GenSeqLike[Int, Int]
    """) === Set(
        "X", "scala.<ref>collection", "scala.collection.<ref>SeqLike",
        "scala.collection.<ref>IterableLike", "scala.collection.<ref>GenSeqLike",
        "scala.<ref>Int", "scala.<ref>AnyRef")
  }

  @Test
  def simple_self_type() = {
    convert("""
      trait X {
        self ⇒
      }
    """) === Set(
        "X", "X.self", "<ref>X", "scala.<ref>AnyRef")
  }

  @Test
  def simple_self_type_to_stdlib_class() = {
    convert("""
      trait X {
        self: scala.collection.mutable.AbstractSet[Int] ⇒
      }
    """) === Set(
        "X", "X.self", "<ref>X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable",
        "scala.collection.mutable.<ref>AbstractSet", "scala.<ref>Int", "scala.<ref>AnyRef")
  }

  @Test
  def simple_self_type_to_stdlib_class_with_full_path_type_argument() = {
    convert("""
      trait X {
        self: scala.collection.mutable.AbstractSet[java.io.File] ⇒
      }
    """) === Set(
        "X", "X.self", "<ref>X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable",
        "scala.collection.mutable.<ref>AbstractSet", "<ref>java", "java.<ref>io", "java.io.<ref>File",
        "scala.<ref>AnyRef")
  }

  @Test
  def self_type_with_nested_types() = {
    convert("""
      trait X {
        self: scala.collection.mutable.AbstractMap[List[Map[Int, Set[Int]]], Map[Int, String]] ⇒
      }
    """) === Set(
        "X", "X.self", "<ref>X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable", "scala.collection.mutable.<ref>AbstractMap",
        "scala.collection.immutable.<ref>List", "scala.collection.immutable.<ref>Map",
        "scala.collection.immutable.<ref>Set", "scala.<ref>Int", "java.lang.<ref>String",
        "scala.<ref>AnyRef")
  }

  @Test
  def complex_self_type() = {
    convert("""
      trait X {
        self: collection.SeqLike[List[Int], List[Int]]
          with collection.IterableLike[List[Int], List[Int]]
          with scala.collection.GenSeqLike[List[Int], List[Int]] ⇒
      }
    """) === Set(
        "X", "X.self", "<ref>X", "scala.collection.<ref>SeqLike", "scala.collection.<ref>IterableLike",
        "scala.collection.<ref>GenSeqLike", "scala.collection.immutable.<ref>List", "scala.<ref>Int",
        "scala.<ref>AnyRef", "scala.<ref>collection", "<ref>scala")
  }

  @Test
  def tuple_as_value() = {
    convert("""
      class X {
        val t = (0, "")
      }
    """) === Set(
        "X", "X.t", "scala.<ref>Int", "java.lang.<ref>String", "scala.<ref>Tuple2",
        "scala.Tuple2.<ref>apply(Ljava/lang/Object;Ljava/lang/Object;)Lscala/Tuple2;",
        "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def tuple_as_parameter_type() = {
    convert("""
      class X {
        def f(t: (Int, String)) = t
      }
    """) === Set(
        "X", "X.f(Lscala/Tuple2;)Lscala/Tuple2;", "X.f(Lscala/Tuple2;)Lscala/Tuple2;.<param>t",
        "X.f(Lscala/Tuple2;)Lscala/Tuple2;.<ref>t", "scala.<ref>Int", "scala.Predef.<ref>String",
        "scala.<ref>Tuple2", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def tuple_type_without_syntactic_sugar() = {
    convert("""
      class X {
        def f(t: Tuple2[Int, String]) = t
      }
    """) === Set(
        "X", "X.f(Lscala/Tuple2;)Lscala/Tuple2;", "X.f(Lscala/Tuple2;)Lscala/Tuple2;.<param>t",
        "X.f(Lscala/Tuple2;)Lscala/Tuple2;.<ref>t", "scala.<ref>Int", "scala.Predef.<ref>String",
        "scala.<ref>Tuple2", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def classOf_reference() = {
    convert("""
      class X {
        val c = classOf[Int]
      }
    """) === Set(
        "X", "X.c", "scala.Predef.<ref>classOf()Ljava/lang/Class;", "scala.<ref>Int",
        "java.lang.<ref>Class", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def private_class_parameter() = {
    convert("""
      class X(i: Int, j: String)
    """) === Set(
        "X", "X.this(ILjava/lang/String;)V", "X.this(ILjava/lang/String;)V.<param>i",
        "X.this(ILjava/lang/String;)V.<param>j", "scala.Predef.<ref>String",
        "scala.<ref>Int", "scala.<ref>AnyRef", "X.<param>i", "X.<param>j")
  }

  @Test
  def public_class_parameter() = {
    convert("""
      class X(val i: Int, val j: String)
    """) === Set(
        "X", "X.this(ILjava/lang/String;)V.<param>i", "X.this(ILjava/lang/String;)V.<param>j",
        "scala.Predef.<ref>String", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this(ILjava/lang/String;)V", "X.<param>i", "X.<param>j")
  }

  @Test
  def class_parameter_with_multiple_argument_lists() = {
    convert("""
      class X(i: Int)(j: String)(k: Int)
    """) === Set(
        "X", "X.this(ILjava/lang/String;I)V.<param>i", "X.this(ILjava/lang/String;I)V.<param>j",
        "X.this(ILjava/lang/String;I)V.<param>k", "scala.Predef.<ref>String",
        "scala.<ref>Int", "scala.<ref>AnyRef", "X.this(ILjava/lang/String;I)V",
        "X.<param>i", "X.<param>j", "X.<param>k")
  }

  @Test
  def method_parameter() = {
    convert("""
      class X {
        def f(i: Int, j: String) = 0
      }
    """) === Set(
        "X", "X.f(ILjava/lang/String;)I", "X.f(ILjava/lang/String;)I.<param>i",
        "X.f(ILjava/lang/String;)I.<param>j", "scala.Predef.<ref>String",
        "scala.<ref>Int", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def method_parameter_with_multiple_argument_lists() = {
    convert("""
      class X {
        def f(i: Int)(j: String)(k: Int) = 0
      }
    """) === Set(
        "X", "X.f(ILjava/lang/String;I)I", "X.f(ILjava/lang/String;I)I.<param>i",
        "X.f(ILjava/lang/String;I)I.<param>j", "X.f(ILjava/lang/String;I)I.<param>k",
        "scala.Predef.<ref>String", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this()V")
  }

  @Test
  def nested_lazy_val() = {
    convert("""
      package pkg
      class X {
        lazy val a = {
          lazy val b = {
            lazy val c = 0
            c
          }
          b
        }
      }
    """) === Set(
        "pkg", "pkg.X", "pkg.X.a", "pkg.X.a.b", "pkg.X.a.b.c", "scala.<ref>Int",
        "pkg.X.a.<ref>b", "pkg.X.a.b.<ref>c", "scala.<ref>AnyRef", "pkg.X.this()V")
  }

  @Test
  def nested_lazy_val_with_special_characters() = {
    convert("""
      package pkg
      class X {
        lazy val ?! = {
          lazy val ??! = {
            lazy val ???! = 0
            ???!
          }
          ??!
        }
      }
    """) === Set(
        "pkg", "pkg.X", "pkg.X.?!", "pkg.X.?!.??!", "pkg.X.?!.??!.???!",
        "scala.<ref>Int", "pkg.X.?!.<ref>??!", "pkg.X.?!.??!.<ref>???!",
        "scala.<ref>AnyRef", "pkg.X.this()V")
  }

  @Test
  def ref_to_name_with_exclamation_marks() = {
    convert("""
      class X {
        def !!(i: Int) = i
        !!(0)
      }
    """) === Set(
        "X", "X.!!(I)I", "X.!!(I)I.<param>i", "X.<ref>!!(I)I", "X.!!(I)I.<ref>i",
        "scala.<ref>Int", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def ref_on_rhs_of_val() = {
    convert("""
      class X {
        val a = 0
        val b = a
      }
    """) === Set(
        "X", "X.a", "X.b", "X.<ref>a", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this()V")
  }

  @Test
  def ref_on_rhs_of_var() = {
    convert("""
      class X {
        var a = 0
        var b = a
      }
    """) === Set(
        "X", "X.a", "X.b", "X.<ref>a", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this()V")
  }

  @Test
  def ref_on_rhs_of_def() = {
    convert("""
      class X {
        def a = 0
        def b = a
      }
    """) === Set(
        "X", "X.a()I", "X.b()I", "X.<ref>a()I", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this()V")
  }

  @Test
  def ref_on_rhs_of_lazy_val() = {
    convert("""
      class X {
        lazy val a = 0
        lazy val b = a
      }
    """) === Set(
        "X", "X.a", "X.b", "X.<ref>a", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this()V")
  }

  @Test
  def rename_import() = {
    convert("""
      import scala.collection.mutable.{Buffer ⇒ B}
      class X {
        B
      }
    """) === Set(
        "X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable",
        "scala.collection.mutable.<ref>B", "scala.collection.mutable.<ref>Buffer",
        "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def rename_import_mixed_with_normal_import() = {
    convert("""
      import scala.collection.mutable.{Buffer ⇒ B, ListBuffer}
      class X {
        B
        ListBuffer
      }
    """) === Set(
        "X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable",
        "scala.collection.mutable.<ref>B", "scala.collection.mutable.<ref>Buffer",
        "scala.collection.mutable.<ref>ListBuffer", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def class_local_rename_import() = {
    convert("""
      class X {
        import scala.collection.mutable.{Buffer ⇒ B}
        B
      }
    """) === Set(
        "X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable",
        "scala.collection.mutable.<ref>B", "scala.collection.mutable.<ref>Buffer",
        "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def local_rename_import() = {
    convert("""
      class X {
        def f = {
          import scala.collection.mutable.{Buffer ⇒ B}
          B
        }
      }
    """) === Set(
        "X", "X.f()Lscala/collection/mutable/Buffer$;", "<ref>scala", "scala.<ref>collection",
        "scala.collection.<ref>mutable", "scala.collection.mutable.<ref>B",
        "scala.collection.mutable.<ref>Buffer", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def explicit_inheritance_to_anyref() = {
    convert("""
      class X extends AnyRef
    """) === Set(
        "X", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def if_expr() = {
    convert("""
      class X {
        val b1 = true
        val b2 = true
        val b3 = true
        def f = if (b1) b2 else b3
      }
    """) === Set(
        "X", "X.b1", "X.b2", "X.b3", "X.f()Z", "X.<ref>b1", "X.<ref>b2",
        "X.<ref>b3", "scala.<ref>Boolean", "scala.<ref>AnyRef", "X.this()V",
        "X.f()Z.<if>", "X.f()Z.<else>")
  }

  @Test
  def overloaded_methods() = {
    convert("""
      class X {
        def f(i: Int) = 0
        def f(i: Int, s: String) = 0
      }
    """) === Set(
        "X", "X.f(I)I", "X.f(I)I.<param>i", "X.f(ILjava/lang/String;)I",
        "X.f(ILjava/lang/String;)I.<param>i", "X.f(ILjava/lang/String;)I.<param>s",
        "scala.<ref>Int", "scala.Predef.<ref>String", "scala.<ref>AnyRef",
        "X.this()V")
  }

  @Test
  def references_in_overloaded_methods() = {
    convert("""
      class X {
        def f(i: Int) = i
        def f(i: Int, s: Float) = i
      }
    """) === Set(
        "X", "scala.<ref>Int", "scala.<ref>Float", "scala.<ref>AnyRef",
        "X.f(I)I", "X.f(I)I.<param>i", "X.f(I)I.<ref>i", "X.this()V",
        "X.f(IF)I", "X.f(IF)I.<param>i", "X.f(IF)I.<param>s", "X.f(IF)I.<ref>i")
  }

  @Test
  def parameter_with_same_name_as_local_value() = {
    convert("""
      class X {
        def f(i: Int) = {
          val i = 0
          i
        }
      }
    """) === Set(
        "X", "X.f(I)I", "X.f(I)I.i", "X.f(I)I.<param>i", "X.f(I)I.<ref>i",
        "scala.<ref>Int", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def def_with_params_and_type_params_of_same_name() = {
    convert("""
      class X {
        def f[A](A: A) = {
          val A = 0
          A
        }
      }
    """) === Set(
        "X", "X.f(Ljava/lang/Object;)I", "X.f(Ljava/lang/Object;)I.<tparam>A",
        "X.f(Ljava/lang/Object;)I.<param>A", "X.f(Ljava/lang/Object;)I.A",
        "X.f(Ljava/lang/Object;)I.<ref>A", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this()V")
  }

  @Test
  def block_in_param_list() = {
    convert("""
      class X {
        def f(i: Int) = 0
        f({val i = 0; i})
      }
    """) === Set(
        "X", "X.f(I)I", "X.f(I)I.<param>i", "scala.<ref>Int",
        "X.<ref>f(I)I", "X.<ref>f(I)I.i", "X.<ref>f(I)I.<ref>i",
        "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def match_expr() = {
    convert("""
      class X {
        val b1 = true
        val b2 = true
        val b3 = true
        val b4 = true
        def f = {
          b1 match {
            case x: Boolean if x == b2 ⇒
              b3
            case _ ⇒
              b4
          }
        }
      }
    """) === Set(
        "X", "X.b1", "X.b2", "X.b3", "X.b4", "X.f()Z",
        "X.f()Z.<match>", "X.f()Z.<match>.<case>", "X.f()Z.<match>.<case>.x",
        "X.f()Z.<match>.<case>.<ref>x", "X.<ref>b1", "X.<ref>b2", "X.<ref>b3", "X.<ref>b4",
        "scala.<ref>Boolean", "scala.Boolean.<ref>==(Z)Z", "scala.<ref>AnyRef",
        "X.this()V")
  }

  @Test
  def match_expr_with_ident_in_backticks() = {
    convert("""
      class X {
        val b1 = true
        def f = {
          val b2 = true
          b1 match {
            case `b2` ⇒
              true
            case _ ⇒
              true
          }
        }
      }
    """) === Set(
        "X", "X.b1", "X.f()Z", "X.f()Z.b2", "X.<ref>b1", "X.f()Z.<ref>b2",
        "X.f()Z.<match>", "X.f()Z.<match>.<case>",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def apply_param_to_apply_method_explicitly() = {
    convert("""
      class X {
        def f(i: Int) = Option.apply(i)
      }
    """) === Set(
        "X", "X.f(I)Lscala/Option;", "X.f(I)Lscala/Option;.<param>i", "X.f(I)Lscala/Option;.<ref>i",
        "scala.<ref>Option", "scala.Option.<ref>apply(Ljava/lang/Object;)Lscala/Option;",
        "scala.<ref>Int", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def match_expr_with_extractor() = {
    convert("""
      class X {
        def f = {
          1 match {
            case Extractor(i) ⇒ i
            case j ⇒ j
          }
        }
      }
      object Extractor {
        def unapply(i: Int) = Option(i)
      }
    """) === Set(
        "X", "X.f()I", "X.f()I.<match>",
        "X.f()I.<match>.<case>", "X.f()I.<match>.<case>.i", "X.f()I.<match>.<case>.j",
        "X.f()I.<match>.<case>.<ref>i", "X.f()I.<match>.<case>.<ref>j",
        "scala.<ref>Int", "<ref>Extractor", "Extractor.<ref>unapply(I)Lscala/Option;",
        "<object>Extractor", "<object>Extractor.unapply(I)Lscala/Option;", "<object>Extractor.unapply(I)Lscala/Option;.<param>i",
        "<object>Extractor.unapply(I)Lscala/Option;.<ref>i", "scala.<ref>Option",
        "scala.Option.<ref>apply(Ljava/lang/Object;)Lscala/Option;", "scala.<ref>AnyRef",
        "X.this()V", "<object>Extractor.this()V")
  }

  @Test
  def try_expr() = {
    convert("""
      class X {
        val b1 = true
        val b2 = true
        val b3 = true
        def f = {
          try b1
          catch {
            case e: Exception ⇒
              b2
          }
          finally println(b3)
        }
      }
    """) === Set(
        "X", "X.b1", "X.b2", "X.b3", "X.f()Z", "X.<ref>b1", "X.<ref>b2", "X.<ref>b3",
        "X.f()Z.<try>", "X.f()Z.<finally>",
        "X.f()Z.<catch>", "X.f()Z.<catch>.<case>", "X.f()Z.<catch>.<case>.e",
        "scala.Predef.<ref>println(Ljava/lang/Object;)V", "scala.<ref>Boolean", "scala.<ref>Exception",
        "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def annotation_declaration() = {
    convert("""
      class Ann(arr: Array[Class[_]]) extends scala.annotation.Annotation
    """) === Set(
        "Ann", "Ann.this([Ljava/lang/Class;)V.<param>arr", "<ref>scala",
        "scala.<ref>annotation", "scala.annotation.<ref>Annotation",
        "scala.<ref>Array", "java.lang.<ref>Class", "Ann.this([Ljava/lang/Class;)V",
        "Ann.<param>arr")
  }

  @Test
  def static_annotation_declaration() = {
    convert("""
      class Ann(arr: Array[Class[_]]) extends scala.annotation.StaticAnnotation
    """) === Set(
        "Ann", "Ann.this([Ljava/lang/Class;)V.<param>arr", "<ref>scala",
        "scala.<ref>annotation", "scala.annotation.<ref>StaticAnnotation",
        "scala.<ref>Array", "java.lang.<ref>Class", "Ann.this([Ljava/lang/Class;)V",
        "Ann.<param>arr")
  }

  @Test
  def class_annotation_without_arguments() = {
    convert("""
      @Ann
      class X
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>AnyRef",
        "X.this()V", "Ann.this()V")
  }

  @Test
  def multiple_class_annotations() = {
    convert("""
      @Ann1 @Ann2
      @Ann3 class X
      class Ann1 extends scala.annotation.StaticAnnotation
      class Ann2 extends scala.annotation.StaticAnnotation
      class Ann3 extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "Ann1", "Ann2", "Ann3", "<ref>Ann1", "<ref>Ann2", "<ref>Ann3",
        "<ref>scala", "scala.<ref>annotation", "scala.annotation.<ref>StaticAnnotation",
        "scala.<ref>AnyRef", "X.this()V", "Ann1.this()V", "Ann2.this()V", "Ann3.this()V")
  }

  @Test
  def class_annotation_with_arguments() = {
    convert("""
      @Ann(Array(classOf[X]))
      class X
      class Ann(arr: Array[Class[_]]) extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "Ann", "Ann.this([Ljava/lang/Class;)V.<param>arr", "<ref>Ann", "scala.Predef.<ref>classOf()Ljava/lang/Class;",
        "<ref>scala", "scala.<ref>annotation", "scala.annotation.<ref>StaticAnnotation",
        "scala.<ref>Array", "java.lang.<ref>Class", "scala.<ref>AnyRef", "<ref>X",
        "scala.Array.<ref>apply(Lscala/collection/Seq;Lscala/reflect/ClassTag;)Ljava/lang/Object;",
        "X.this()V", "Ann.this([Ljava/lang/Class;)V", "Ann.<param>arr",
        "scala.reflect.<ref>materializeClassTag()Lscala/reflect/ClassTag;")
  }

  @Test
  def class_annotation_with_constant_argument() = {
    convert("""
      @Ann(Constants.C)
      class X
      object Constants {
        final val C = 0
      }
      class Ann(v: Int) extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "Ann", "<object>Constants", "<object>Constants.C", "Ann.this(I)V.<param>v", "<ref>Ann",
        "<ref>Constants", "Constants.<ref>C", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int",
        "scala.<ref>AnyRef", "X.this()V", "<object>Constants.this()V", "Ann.this(I)V",
        "Ann.<param>v")
  }

  @Test
  def object_annotation_without_arguments() = {
    convert("""
      @Ann
      object X
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "<object>X", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>AnyRef",
        "<object>X.this()V", "Ann.this()V")
  }

  @Test
  def def_annotation_without_arguments() = {
    convert("""
      class X {
        @Ann
        def f = 0
      }
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "X.f()I", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this()V", "Ann.this()V")
  }

  @Test
  def val_annotation_without_arguments() = {
    convert("""
      class X {
        @Ann
        val v = 0
      }
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "X.v", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this()V", "Ann.this()V")
  }

  @Test
  def var_annotation_without_arguments() = {
    convert("""
      class X {
        @Ann
        var v = 0
      }
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "X.v", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this()V", "Ann.this()V")
  }

  @Test
  def param_annotation_without_arguments() = {
    convert("""
      class X {
        def f(@Ann i: Int) = i
      }
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "X.f(I)I", "X.f(I)I.<param>i", "X.f(I)I.<ref>i", "Ann", "<ref>Ann",
        "<ref>scala", "scala.<ref>annotation", "scala.annotation.<ref>StaticAnnotation",
        "scala.<ref>Int", "scala.<ref>AnyRef", "X.this()V", "Ann.this()V")
  }

  @Test
  def lazy_val_annotation_without_arguments() = {
    convert("""
      class X {
        @Ann
        lazy val v = 0
      }
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "X.v", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this()V", "Ann.this()V")
  }

  @Test
  def ctor_param_annotation_without_arguments() = {
    convert("""
      class X(@Ann p: Int)
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "X.this(I)V.<param>p", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int", "scala.<ref>AnyRef",
        "X.this(I)V", "Ann.this()V", "X.<param>p")
  }

  @Test
  def auxiliary_ctor_annotation_without_arguments() = {
    convert("""
      class X(p: Int) {
        @Ann
        def this() = this(0)
      }
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "X.this(I)V.<param>p", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "X.this()V", "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int",
        "scala.<ref>AnyRef", "X.<ref>this(I)V", "X.this(I)V", "Ann.this()V",
        "X.<param>p")
  }

  @Test
  def throw_exception() = {
    convert("""
      class X {
        def f: Int = throw new IllegalArgumentException
      }
    """) === Set(
        "X", "X.f()I", "scala.<ref>IllegalArgumentException", "scala.<ref>Int",
        "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def auxiliary_ctor() = {
    convert("""
      class X(p: Int) {
        def this(a: Int, b: Int) = {
          this(a+b)
        }
      }
    """) === Set(
        "X", "X.this(I)V.<param>p", "scala.<ref>Int", "scala.Int.<ref>+(I)I", "X.this(II)V",
        "X.this(II)V.<param>a", "X.this(II)V.<param>b", "X.this(II)V.<ref>a",
        "X.this(II)V.<ref>b", "scala.<ref>AnyRef", "X.<ref>this(I)V", "X.this(I)V",
        "X.<param>p")
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
        "X", "X.f()I", "X.f()I.x", "X.f()I.<ref>x", "scala.<ref>Int",
        "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def this_ref() = {
    convert("""
      class X {
        val value = this
      }
    """) === Set(
        "X", "X.value", "<ref>X", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def type_alias() = {
    convert("""
      class X {
        type Type = X
      }
    """) === Set(
        "X", "X.Type", "<ref>X", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def type_alias_with_type_parameter() = {
    convert("""
      class X {
        type Type[A, B] = Map[A, B]
      }
    """) === Set(
        "X", "X.Type", "X.Type.<tparam>A", "X.Type.<tparam>B",
        "X.Type.<ref>A", "X.Type.<ref>B",
        "scala.<ref>AnyRef", "X.this()V", "scala.Predef.<ref>Map")
  }

  @Test
  def if_scope() = {
    convert("""
      class X {
        def f = {
          if (true) {
            val b = true
            b
          }
          true
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<if>", "X.f()Z.<if>.b", "X.f()Z.<if>.<ref>b",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def nested_if_scope() = {
    convert("""
      class X {
        def f = {
          if (true) {
            val b = true
            if (true) {
              b
            }
          }
          true
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<if>", "X.f()Z.<if>.b", "X.f()Z.<if>.<if>", "X.f()Z.<if>.<ref>b",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def variable_shadowing_in_nested_if_scope() = {
    convert("""
      class X {
        def f = {
          if (true) {
            val b = true
            if (true) {
              val b = true
              b
            }
          }
          true
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<if>", "X.f()Z.<if>.b", "X.f()Z.<if>.<if>",
        "X.f()Z.<if>.<if>.b", "X.f()Z.<if>.<if>.<ref>b",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def else_scope() = {
    convert("""
      class X {
        def f = {
          if (true)
            true
          else {
            val b = true
            b
          }
          true
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<if>", "X.f()Z.<else>", "X.f()Z.<else>.b", "X.f()Z.<else>.<ref>b",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def try_scope() = {
    convert("""
      class X {
        def f = {
          try {
            val b = true
            b
          }
          catch {
            case _: Throwable ⇒
          }
          true
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<try>", "X.f()Z.<try>.b", "X.f()Z.<try>.<ref>b",
        "X.f()Z.<catch>", "X.f()Z.<catch>.<case>",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "scala.<ref>Throwable", "X.this()V")
  }

  @Test
  def finally_scope() = {
    convert("""
      class X {
        def f = {
          try {
            true
          }
          catch {
            case _: Throwable ⇒
          }
          finally {
            val b = true
            println(b)
          }
          true
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<try>", "X.f()Z.<finally>", "X.f()Z.<finally>.b",
        "X.f()Z.<catch>", "X.f()Z.<catch>.<case>",
        "X.f()Z.<finally>.<ref>b", "scala.Predef.<ref>println(Ljava/lang/Object;)V",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "scala.<ref>Throwable", "X.this()V")
  }

  @Test
  def catch_scope() = {
    convert("""
      class X {
        def f = {
          try {
            true
          }
          catch {
            case _: Throwable ⇒
              val b = true
              b
          }
          true
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<try>", "X.f()Z.<catch>", "X.f()Z.<catch>.<case>",
        "X.f()Z.<catch>.<case>.b", "X.f()Z.<catch>.<case>.<ref>b",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "scala.<ref>Throwable", "X.this()V")
  }

  @Test
  def try_finally_scope() = {
    convert("""
      class X {
        def f = {
          try {
            val b = true
            b
          }
          finally {
          }
          true
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<try>", "X.f()Z.<try>.b", "X.f()Z.<try>.<ref>b",
        "X.f()Z.<finally>", "scala.<ref>Boolean", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def match_scope() = {
    convert("""
      class X {
        def f = {
          true match {
            case _ ⇒
              val b = true
              b
          }
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<match>", "X.f()Z.<match>.<case>",
        "X.f()Z.<match>.<case>.b", "X.f()Z.<match>.<case>.<ref>b",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def decl_in_case_scope_is_correctly_referenced() = {
    convert("""
      class X {
        def f = {
          true match {
            case b ⇒
              b
          }
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<match>", "X.f()Z.<match>.<case>",
        "X.f()Z.<match>.<case>.b", "X.f()Z.<match>.<case>.<ref>b",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def while_scope() = {
    convert("""
      class X {
        def f = {
          while (true) {
            val b = true
            println(b)
          }
          true
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<while>", "X.f()Z.<while>.b",
        "X.f()Z.<while>.<ref>b", "scala.Predef.<ref>println(Ljava/lang/Object;)V",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def do_while_scope() = {
    convert("""
      class X {
        def f = {
          do {
            val b = true
            println(b)
          } while (false)
          true
        }
      }
    """) === Set(
        "X", "X.f()Z", "X.f()Z.<do>", "X.f()Z.<do>.b",
        "X.f()Z.<do>.<ref>b", "scala.Predef.<ref>println(Ljava/lang/Object;)V",
        "scala.<ref>Boolean", "scala.<ref>AnyRef", "X.this()V")
  }

  @Test
  def class_and_object() = {
    convert("""
      class X
      object X
      class Y {
        def cls: X = null
        def obj: X.type = null
      }
    """) === Set(
        "X", "<object>X", "X.this()V", "<object>X.this()V", "Y", "Y.this()V",
        "Y.cls()LX;", "Y.obj()LX$;", "<ref>X", "scala.<ref>AnyRef")
  }

  @Test
  def class_and_object_with_same_member_name() = {
    convert("""
      class X {
        val value = 0
      }
      object X {
        val value = 0
      }
    """) === Set(
        "X", "X.this()V", "X.value", "scala.<ref>AnyRef", "scala.<ref>Int",
        "<object>X", "<object>X.this()V", "<object>X.value")
  }

  @Test
  def implicit_val() = {
    convert("""
      class X {
        implicit val value = 0
      }
    """) === Set(
        "X", "X.this()V", "X.value", "scala.<ref>AnyRef", "scala.<ref>Int")
  }

  @Test
  def case_class() = {
    convert("""
      case class X(i: Int)
    """) === Set(
        "<object>X",
        "<object>X.apply(I)LX;",
        "<object>X.apply(I)LX;.<param>i",
        "<object>X.readResolve()Ljava/lang/Object;",
        "<object>X.this()V",
        "<object>X.toString()Ljava/lang/String;",
        "<object>X.unapply(LX;)Lscala/Option;",
        "<ref>X",
        "X",
        "X.<param>i",
        "X.canEqual(Ljava/lang/Object;)Z",
        "X.copy$default$1()I",
        "X.copy(I)LX;",
        "X.copy(I)LX;.<param>i",
        "X.equals(Ljava/lang/Object;)Z",
        "X.hashCode()I",
        "X.productArity()I",
        "X.productElement(I)Ljava/lang/Object;",
        "X.productIterator()Lscala/collection/Iterator;",
        "X.productPrefix()Ljava/lang/String;",
        "X.this(I)V",
        "X.this(I)V.<param>i",
        "X.toString()Ljava/lang/String;",
        "java.lang.<ref>String",
        "scala.<ref>Any",
        "scala.<ref>AnyRef",
        "scala.<ref>Boolean",
        "scala.<ref>Int",
        "scala.<ref>Option",
        "scala.<ref>Product",
        "scala.<ref>Serializable",
        "scala.collection.<ref>Iterator",
        "scala.runtime.<ref>AbstractFunction1")
  }

  @Test
  def type_ascription() = {
    convert("""
      class X {
        println(1): Unit
      }
    """) === Set(
        "X", "X.this()V", "scala.<ref>AnyRef", "scala.<ref>Unit", "scala.Predef.<ref>println(Ljava/lang/Object;)V")
  }

  @Test
  def function_application() = {
    convert("""
      class X {
        def f(i: Int) = 0
        def g = f _
      }
    """) === Set(
        "X", "X.this()V", "X.f(I)I", "X.f(I)I.<param>i", "X.<ref>f(I)I", "X.<ref>f(I)I.<ref>i",
        "X.g()Lscala/Function1;",
        "scala.<ref>AnyRef", "scala.<ref>Function1", "scala.<ref>Int")
  }

  @Test
  def partial_function_application() = {
    convert("""
      class X {
        def f(pf: PartialFunction[Int, Int]) = 0
        f {
          case 0 ⇒
            val value = 0
            value
        }
      }
    """) === Set(
        "X",
        "X.<ref>f(Lscala/PartialFunction;)I",
        "X.<ref>f(Lscala/PartialFunction;)I.<case>",
        "X.<ref>f(Lscala/PartialFunction;)I.<case>.value",
        "X.<ref>f(Lscala/PartialFunction;)I.<case>.<ref>value",
        "X.f(Lscala/PartialFunction;)I",
        "X.f(Lscala/PartialFunction;)I.<param>pf",
        "X.this()V",
        "scala.<ref>AnyRef",
        "scala.<ref>Int",
        "scala.<ref>PartialFunction")
  }

  @Test
  def classOf_application() = {
    convert("""
      class X {
        classOf[String].getClass
      }
    """) === Set(
        "X",
        "X.this()V",
        "java.lang.<ref>String",
        "java.lang.Object.<ref>getClass()Ljava/lang/Class;",
        "scala.<ref>AnyRef",
        "scala.Predef.<ref>classOf()Ljava/lang/Class;")
  }

  @Test
  def string_interpolation() = {
    convert("""
      class X {
        val x1 = 0
        val x2 = 0
        val y = s"$x1$x2"
      }
    """) === Set(
        "X",
        "X.<ref>x1",
        "X.<ref>x2",
        "X.this()V",
        "X.x1",
        "X.x2",
        "X.y",
        "scala.<ref>AnyRef",
        "scala.<ref>Int",
        "scala.Predef.<ref>String",
        "scala.StringContext.<ref>s(Lscala/collection/Seq;)Ljava/lang/String;")
  }

  @Test
  def case_class_unapply_method() = {
    convert("""
      class X {
        def f(h: Option[Int]) = h match {
          case Some(i) ⇒ i
        }
      }
    """) === Set(
        "X",
        "X.f(Lscala/Option;)I",
        "X.f(Lscala/Option;)I.<match>",
        "X.f(Lscala/Option;)I.<match>.<case>",
        "X.f(Lscala/Option;)I.<param>h",
        "X.f(Lscala/Option;)I.<ref>h",
        "X.this()V",
        "scala.<ref>AnyRef",
        "scala.<ref>Int",
        "scala.<ref>Option",
        "scala.<ref>Some",
        "scala.<ref>Some.<ref>i",
        "scala.<ref>Some.i")
  }

  @Test
  def alternative() = {
    convert("""
      class X {
        def f(i: Int) = i match {
          case 1 | 2 ⇒ i
        }
      }
    """) === Set(
        "X",
        "X.f(I)I",
        "X.f(I)I.<match>",
        "X.f(I)I.<match>.<case>",
        "X.f(I)I.<param>i",
        "X.f(I)I.<ref>i",
        "X.this()V",
        "scala.<ref>AnyRef",
        "scala.<ref>Int")
  }

  @Test
  def repeated_args() = {
    convert("""
      class X {
        def f(i: Int*) = 0
      }
    """) === Set(
        "X",
        "X.f(Lscala/collection/Seq;)I",
        "X.f(Lscala/collection/Seq;)I.<param>i",
        "X.this()V",
        "scala.<ref>AnyRef",
        "scala.<ref>Int")
  }
}
