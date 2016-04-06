package plugin

import org.junit.Test

import indexer.hierarchy.Root

class ScalacConverterTest {

  import TestUtils._

  def convert(src: String): Set[String] =
    convert("<memory>" → src)

  def convert(data: (String, String)*): Set[String] = {
    val res = convertToHierarchy(data: _*).flatMap(_._2)
    res.map(_.asString).map(_.drop(Root.name.length+1)).toSet
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
    """) === Set("pkg", "pkg.X", "pkg.X.a()I", "scala.<ref>Int")
  }

  @Test
  def single_val() = {
    convert("""
      package pkg
      class X {
        val a = 0
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "scala.<ref>Int")
  }

  @Test
  def single_lazy_val() = {
    convert("""
      package pkg
      class X {
        lazy val a = 0
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "scala.<ref>Int")
  }

  @Test
  def single_var() = {
    convert("""
      package pkg
      class X {
        var a = 0
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a", "scala.<ref>Int")
  }

  @Test
  def getter_and_setter() = {
    convert("""
      package pkg
      class X {
        def a = 0
        def a_=(a: Int) = ()
      }
    """) === Set("pkg", "pkg.X", "pkg.X.a()I", "pkg.X.a_=(I)V", "pkg.X.a_=(I)V.<param>a", "scala.<ref>Int", "scala.<ref>Unit")
  }

  @Test
  def names_with_special_characters() = {
    convert("""
      package pkg
      class X_? {
        val !!! = 0
        def ??? = 0
      }
    """) === Set("pkg", "pkg.X_?", "pkg.X_?.!!!", "pkg.X_?.???()I", "scala.<ref>Int")
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
        "pkg.`A B C`.`a b c`", "pkg.`A B C`.`d e f`()I", "pkg.`A B C`.`type`()I")
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
        "pkg.X.a()I.b()I.c.d", "scala.<ref>Int", "pkg.X.a()I.<ref>b", "pkg.X.a()I.b()I.<ref>c", "pkg.X.a()I.b()I.c.<ref>d")
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
    """) === Set("pkg", "pkg.X", "pkg.X.Y", "pkg.X.Y.Z", "pkg.X.Y.Z.a", "pkg.X.Y.Z.b()I", "scala.<ref>Int")
  }

  @Test
  def simple_ref() = {
    convert("""
      package pkg
      class X {
        toString
      }
    """) === Set("pkg", "pkg.X", "java.lang.Object.<ref>toString")
  }

  @Test
  def chained_ref() = {
    convert("""
      package pkg
      class X {
        toString.toString.toString.toString
      }
    """) === Set("pkg", "pkg.X", "java.lang.Object.<ref>toString", "java.lang.String.<ref>toString")
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
    """) === Set("a.b.c.d", "a.b.c.d.X", "a.b.c.d.X.x()I", "scala.<ref>Int")
  }

  @Test
  def empty_package() = {
    convert("""
      class X {
        def x = 0
      }
    """) === Set("X", "X.x()I", "scala.<ref>Int")
  }

  @Test
  def single_import() = {
    convert("""
      import scala.collection.mutable.ListBuffer
      class X {
        ListBuffer
      }
    """) === Set("X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable", "scala.collection.mutable.<ref>ListBuffer")
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
        "<ref>java", "java.<ref>io", "java.io.<ref>File")
  }

  @Test
  def type_parameter_at_classes() = {
    convert("""
      class X[A, B]
    """) === Set("X", "X.<tparam>A", "X.<tparam>B")
  }

  @Test
  def type_parameter_at_methods() = {
    convert("""
      class X {
        def f[A, B] = 0
      }
    """) === Set("X", "X.f()I", "X.f()I.<tparam>A", "X.f()I.<tparam>B", "scala.<ref>Int")
  }

  @Test
  def type_parameter_at_type_ascriptions() = {
    convert("""
      class X {
        def f: Option[Int] = null
      }
    """) === Set("X", "X.f()Lscala/Option;", "scala.<ref>Option", "scala.<ref>Int")
  }

  @Test
  def apply_method_implicitly() = {
    convert("""
      class X {
        def f = Option(1)
      }
    """) === Set("X", "X.f()Lscala/Option;", "scala.<ref>Option", "scala.Option.<ref>apply", "scala.<ref>Int")
  }

  @Test
  def apply_method_explicitly() = {
    convert("""
      class X {
        def f = Option.apply(1)
      }
    """) === Set("X", "X.f()Lscala/Option;", "scala.<ref>Option", "scala.Option.<ref>apply", "scala.<ref>Int")
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
        "X.f(ILjava/lang/String;)I.g(I)I.<param>i", "scala.<ref>Int", "scala.Predef.<ref>String")
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
    """) === Set("X", "X.v", "X.f(I)I", "X.f(I)I.<param>i", "X.<ref>f", "X.f(I)I.<ref>i", "X.<ref>v", "scala.<ref>Int", "scala.Int.<ref>MinValue")
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
    """) === Set("X", "<ref>scala", "scala.<ref>util")
  }

  @Test
  def wildcard_import_in_class_body() = {
    convert("""
      class X {
        import scala.util._
      }
    """) === Set("X", "<ref>scala", "scala.<ref>util")
  }

  @Test
  def new_object() = {
    convert("""
      class X {
        val x = new Object
      }
    """) === Set("X", "X.x", "java.lang.<ref>Object")
  }

  @Test
  def method_call_in_body() = {
    convert("""
      class X {
        def f = {
          toString
        }
      }
    """) === Set("X", "X.f()Ljava/lang/String;", "java.lang.Object.<ref>toString", "java.lang.<ref>String")
  }

  @Test
  def simple_lambda() = {
    convert("""
      class X {
        def meth(f: Int ⇒ Int) = 0
      }
    """) === Set(
        "X", "X.meth(Lscala/Function1;)I", "X.meth(Lscala/Function1;)I.<param>f",
        "scala.<ref>Function1", "scala.<ref>Int")
  }

  @Test
  def call_by_name_param() = {
    convert("""
      class X {
        def meth(f: ⇒ Int) = 0
      }
    """) === Set("X", "X.meth(Lscala/Function0;)I", "X.meth(Lscala/Function0;)I.<param>f", "scala.<ref>Function0", "scala.<ref>Int")
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
        "X.<ref>meth", "X.<ref>meth.<param>v", "X.<ref>meth.<ref>v", "scala.<ref>Function1", "scala.<ref>Int")
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
    """) === Set("a", "a.X", "a.X.m()Lb/Y;", "b", "b.Y", "<ref>b", "b.<ref>Y")
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
        "X.<ref>meth", "X.<ref>meth.<param>a", "X.<ref>meth.<param>b", "X.<ref>meth.<param>c",
        "scala.<ref>Function3", "scala.<ref>Int")
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
        "X.<ref>meth", "X.<ref>meth.<param>a", "X.<ref>meth.<param>b", "X.<ref>meth.<param>c",
        "scala.<ref>Function1", "scala.<ref>Int")
  }

  @Test
  def simple_inheritance_in_single_file() = {
    convert("""
      class X
      class Y extends X
    """) === Set("X", "Y", "<ref>X")
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
    """) === Set("a", "a.X", "b", "b.Y", "<ref>b", "b.<ref>Y")
  }

  @Test
  def simple_inheritance_from_stdlib_class() = {
    convert("""
      trait X extends scala.collection.mutable.AbstractSet[Int]
    """) === Set("X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable", "scala.collection.mutable.<ref>AbstractSet", "scala.<ref>Int")
  }

  @Test
  def simple_inheritance_from_multiple_stdlib_classes() = {
    convert("""
      trait X extends collection.SeqLike[Int, Int]
        with collection.IterableLike[Int, Int]
        with collection.GenSeqLike[Int, Int]
    """) === Set("X", "scala.<ref>collection", "scala.collection.<ref>SeqLike", "scala.collection.<ref>IterableLike", "scala.collection.<ref>GenSeqLike", "scala.<ref>Int")
  }

  @Test
  def simple_self_type() = {
    convert("""
      trait X {
        self ⇒
      }
    """) === Set("X", "X.self", "<ref>X")
  }

  @Test
  def simple_self_type_to_stdlib_class() = {
    convert("""
      trait X {
        self: scala.collection.mutable.AbstractSet[Int] ⇒
      }
    """) === Set(
        "X", "X.self", "<ref>X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable",
        "scala.collection.mutable.<ref>AbstractSet", "scala.<ref>Int")
  }

  @Test
  def simple_self_type_to_stdlib_class_with_full_path_type_argument() = {
    convert("""
      trait X {
        self: scala.collection.mutable.AbstractSet[java.io.File] ⇒
      }
    """) === Set(
        "X", "X.self", "<ref>X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable",
        "scala.collection.mutable.<ref>AbstractSet", "<ref>java", "java.<ref>io", "java.io.<ref>File")
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
        "scala.collection.immutable.<ref>Set", "scala.<ref>Int", "java.lang.<ref>String")
  }

  @Test
  def complex_self_type() = {
    convert("""
      trait X {
        self: collection.SeqLike[List[Int], List[Int]]
          with collection.IterableLike[List[Int], List[Int]]
          with collection.GenSeqLike[List[Int], List[Int]] ⇒
      }
    """) === Set(
        "X", "X.self", "<ref>X", "scala.collection.<ref>SeqLike", "scala.collection.<ref>IterableLike",
        "scala.collection.<ref>GenSeqLike", "scala.collection.immutable.<ref>List", "scala.<ref>Int")
  }

  @Test
  def tuple_as_value() = {
    convert("""
      class X {
        val t = (0, "")
      }
    """) === Set("X", "X.t", "scala.<ref>Int", "java.lang.<ref>String", "scala.<ref>Tuple2", "scala.Tuple2.<ref>apply")
  }

  @Test
  def tuple_as_parameter_type() = {
    convert("""
      class X {
        def f(t: (Int, String)) = t
      }
    """) === Set(
        "X", "X.f(Lscala/Tuple2;)Lscala/Tuple2;", "X.f(Lscala/Tuple2;)Lscala/Tuple2;.<param>t",
        "X.f(Lscala/Tuple2;)Lscala/Tuple2;.<ref>t", "scala.<ref>Int", "scala.Predef.<ref>String", "scala.<ref>Tuple2")
  }

  @Test
  def tuple_type_without_syntactic_sugar() = {
    convert("""
      class X {
        def f(t: Tuple2[Int, String]) = t
      }
    """) === Set(
        "X", "X.f(Lscala/Tuple2;)Lscala/Tuple2;", "X.f(Lscala/Tuple2;)Lscala/Tuple2;.<param>t",
        "X.f(Lscala/Tuple2;)Lscala/Tuple2;.<ref>t", "scala.<ref>Int", "scala.Predef.<ref>String", "scala.<ref>Tuple2")
  }

  @Test
  def classOf_reference() = {
    convert("""
      class X {
        val c = classOf[Int]
      }
    """) === Set("X", "X.c", "scala.Predef.<ref>classOf", "scala.<ref>Int", "java.lang.<ref>Class")
  }

  @Test
  def private_class_parameter() = {
    convert("""
      class X(i: Int, j: String)
    """) === Set("X", "X.<param>i", "X.<param>j", "scala.Predef.<ref>String", "scala.<ref>Int")
  }

  @Test
  def public_class_parameter() = {
    convert("""
      class X(val i: Int, val j: String)
    """) === Set("X", "X.<param>i", "X.<param>j", "scala.Predef.<ref>String", "scala.<ref>Int")
  }

  @Test
  def class_parameter_with_multiple_argument_lists() = {
    convert("""
      class X(i: Int)(j: String)(k: Int)
    """) === Set("X", "X.<param>i", "X.<param>j", "X.<param>k", "scala.Predef.<ref>String", "scala.<ref>Int")
  }

  @Test
  def method_parameter() = {
    convert("""
      class X {
        def f(i: Int, j: String) = 0
      }
    """) === Set(
        "X", "X.f(ILjava/lang/String;)I", "X.f(ILjava/lang/String;)I.<param>i",
        "X.f(ILjava/lang/String;)I.<param>j", "scala.Predef.<ref>String", "scala.<ref>Int")
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
        "scala.Predef.<ref>String", "scala.<ref>Int")
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
    """) === Set("pkg", "pkg.X", "pkg.X.a", "pkg.X.a.b", "pkg.X.a.b.c", "scala.<ref>Int", "pkg.X.a.<ref>b", "pkg.X.a.b.<ref>c")
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
    """) === Set("pkg", "pkg.X", "pkg.X.?!", "pkg.X.?!.??!", "pkg.X.?!.??!.???!", "scala.<ref>Int", "pkg.X.?!.<ref>??!", "pkg.X.?!.??!.<ref>???!")
  }

  @Test
  def ref_to_name_with_exclamation_marks() = {
    convert("""
      class X {
        def !!(i: Int) = i
        !!(0)
      }
    """) === Set("X", "X.!!(I)I", "X.!!(I)I.<param>i", "X.<ref>!!", "X.!!(I)I.<ref>i", "scala.<ref>Int")
  }

  @Test
  def ref_on_rhs_of_val() = {
    convert("""
      class X {
        val a = 0
        val b = a
      }
    """) === Set("X", "X.a", "X.b", "X.<ref>a", "scala.<ref>Int")
  }

  @Test
  def ref_on_rhs_of_var() = {
    convert("""
      class X {
        var a = 0
        var b = a
      }
    """) === Set("X", "X.a", "X.b", "X.<ref>a", "scala.<ref>Int")
  }

  @Test
  def ref_on_rhs_of_def() = {
    convert("""
      class X {
        def a = 0
        def b = a
      }
    """) === Set("X", "X.a()I", "X.b()I", "X.<ref>a", "scala.<ref>Int")
  }

  @Test
  def ref_on_rhs_of_lazy_val() = {
    convert("""
      class X {
        lazy val a = 0
        lazy val b = a
      }
    """) === Set("X", "X.a", "X.b", "X.<ref>a", "scala.<ref>Int")
  }

  @Test
  def rename_import() = {
    convert("""
      import scala.collection.mutable.{Buffer ⇒ B}
      class X {
        B
      }
    """) === Set("X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable", "scala.collection.mutable.<ref>B", "scala.collection.mutable.<ref>Buffer")
  }

  @Test
  def rename_import_mixed_with_normal_import() = {
    convert("""
      import scala.collection.mutable.{Buffer ⇒ B, ListBuffer}
      class X {
        B
        ListBuffer
      }
    """) === Set("X", "<ref>scala", "scala.<ref>collection", "scala.collection.<ref>mutable", "scala.collection.mutable.<ref>B", "scala.collection.mutable.<ref>Buffer", "scala.collection.mutable.<ref>ListBuffer")
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
        "scala.collection.mutable.<ref>B", "scala.collection.mutable.<ref>Buffer")
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
        "X", "X.f()Lscala/collection/mutable/Buffer;", "<ref>scala", "scala.<ref>collection",
        "scala.collection.<ref>mutable", "scala.collection.mutable.<ref>B", "scala.collection.mutable.<ref>Buffer")
  }

  @Test
  def explicit_inheritance_to_anyref() = {
    convert("""
      class X extends AnyRef
    """) === Set("X", "scala.<ref>AnyRef")
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
    """) === Set("X", "X.b1", "X.b2", "X.b3", "X.f()Z", "X.<ref>b1", "X.<ref>b2", "X.<ref>b3", "scala.<ref>Boolean")
  }

  @Test
  def overloaded_methods() = {
    convert("""
      class X {
        def f(i: Int) = 0
        def f(i: Int, s: String) = 0
      }
    """) === Set(
        "X", "X.f(I)I", "X.f(I)I.<param>i", "X.f(ILjava/lang/String;)I", "X.f(ILjava/lang/String;)I.<param>i",
        "X.f(ILjava/lang/String;)I.<param>s", "scala.<ref>Int", "scala.Predef.<ref>String")
  }

  @Test
  def references_in_overloaded_methods() = {
    convert("""
      class X {
        def f(i: Int) = i
        def f(i: Int, s: Float) = i
      }
    """) === Set(
        "X", "scala.<ref>Int", "scala.<ref>Float",
        "X.f(I)I", "X.f(I)I.<param>i", "X.f(I)I.<ref>i",
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
    """) === Set("X", "X.f(I)I", "X.f(I)I.i", "X.f(I)I.<param>i", "X.f(I)I.<ref>i", "scala.<ref>Int")
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
        "X.f(Ljava/lang/Object;)I.<ref>A", "scala.<ref>Int")
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
        "X.<ref>f", "X.<ref>f.i", "X.<ref>f.<ref>i")
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
        "X", "X.b1", "X.b2", "X.b3", "X.b4", "X.f()Z", "X.f()Z.x",
        "X.<ref>b1", "X.<ref>b2", "X.<ref>b3", "X.<ref>b4", "X.f()Z.<ref>x", "scala.<ref>Boolean", "scala.Boolean.<ref>==")
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
    """) === Set("X", "X.b1", "X.f()Z", "X.f()Z.b2", "X.<ref>b1", "X.f()Z.<ref>b2", "scala.<ref>Boolean")
  }

  @Test
  def apply_param_to_apply_method_explicitly() = {
    convert("""
      class X {
        def f(i: Int) = Option.apply(i)
      }
    """) === Set(
        "X", "X.f(I)Lscala/Option;", "X.f(I)Lscala/Option;.<param>i", "X.f(I)Lscala/Option;.<ref>i",
        "scala.<ref>Option", "scala.Option.<ref>apply", "scala.<ref>Int")
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
        "X", "X.f()I", "X.f()I.i", "X.f()I.j", "X.f()I.<ref>i", "X.f()I.<ref>j",
        "scala.<ref>Int", "<ref>Extractor", "Extractor.<ref>unapply", "Extractor",
        "Extractor.unapply(I)Lscala/Option;", "Extractor.unapply(I)Lscala/Option;.<param>i",
        "Extractor.unapply(I)Lscala/Option;.<ref>i", "scala.<ref>Option", "scala.Option.<ref>apply")
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
        "X", "X.b1", "X.b2", "X.b3", "X.f()Z", "X.f()Z.e", "X.<ref>b1", "X.<ref>b2", "X.<ref>b3",
        "scala.Predef.<ref>println", "scala.<ref>Boolean", "scala.<ref>Exception")
  }

  @Test
  def annotation_declaration() = {
    convert("""
      class Ann(arr: Array[Class[_]]) extends scala.annotation.Annotation
    """) === Set(
        "Ann", "Ann.<param>arr", "<ref>scala", "scala.<ref>annotation", "scala.annotation.<ref>Annotation",
        "scala.<ref>Array", "java.lang.<ref>Class")
  }

  @Test
  def static_annotation_declaration() = {
    convert("""
      class Ann(arr: Array[Class[_]]) extends scala.annotation.StaticAnnotation
    """) === Set(
        "Ann", "Ann.<param>arr", "<ref>scala", "scala.<ref>annotation", "scala.annotation.<ref>StaticAnnotation",
        "scala.<ref>Array", "java.lang.<ref>Class")
  }

  @Test
  def class_annotation_without_arguments() = {
    convert("""
      @Ann
      class X
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation")
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
        "<ref>scala", "scala.<ref>annotation", "scala.annotation.<ref>StaticAnnotation")
  }

  @Test
  def class_annotation_with_arguments() = {
    convert("""
      @Ann(Array(classOf[X]))
      class X
      class Ann(arr: Array[Class[_]]) extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "Ann", "Ann.<param>arr", "<ref>Ann", "scala.Predef.<ref>classOf", "<ref>X",
        "<ref>scala", "scala.<ref>annotation", "scala.annotation.<ref>StaticAnnotation",
        "scala.<ref>Array", "scala.Array.<ref>apply", "java.lang.<ref>Class")
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
        "X", "Ann", "Constants", "Constants.C", "Ann.<param>v", "<ref>Ann", "<ref>Constants", "Constants.<ref>C",
        "<ref>scala", "scala.<ref>annotation", "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int")
  }

  @Test
  def object_annotation_without_arguments() = {
    convert("""
      @Ann
      object X
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation")
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
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int")
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
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int")
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
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int")
  }

  @Test
  def param_annotation_without_arguments() = {
    convert("""
      class X {
        def f(@Ann i: Int) = i
      }
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "X.f(I)I", "X.f(I)I.<param>i", "X.f(I)I.<ref>i", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int")
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
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int")
  }

  @Test
  def ctor_param_annotation_without_arguments() = {
    convert("""
      class X(@Ann p: Int)
      class Ann extends scala.annotation.StaticAnnotation
    """) === Set(
        "X", "X.<param>p", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int")
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
        "X", "X.<param>p", "Ann", "<ref>Ann", "<ref>scala", "scala.<ref>annotation",
        "X.this()V", "scala.annotation.<ref>StaticAnnotation", "scala.<ref>Int")
  }

  @Test
  def throw_exception() = {
    convert("""
      class X {
        def f: Int = throw new IllegalArgumentException
      }
    """) === Set(
        "X", "X.f()I", "scala.<ref>IllegalArgumentException", "scala.<ref>Int")
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
        "X", "X.<param>p", "scala.<ref>Int", "scala.Int.<ref>+", "X.this(II)V",
        "X.this(II)V.<param>a", "X.this(II)V.<param>b", "X.this(II)V.<ref>a", "X.this(II)V.<ref>b")
  }

}
