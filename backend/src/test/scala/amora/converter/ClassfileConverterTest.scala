package amora.converter

import org.junit.Test

import amora.backend.indexer.JavaBytecodeIndexer
import amora.backend.TestUtils
import amora.backend.IgnoreLogger

class ClassfileConverterTest {

  import TestUtils._

  def convert(src: String): Set[String] =
    convert("<memory>" → src)

  def convert(data: (String, String)*): Set[String] = {
    val indexer = new JavaBytecodeIndexer(IgnoreLogger)
    val res = indexer.bytecodeToHierarchy(data) match {
      case scala.util.Success(res) ⇒ res.flatMap(_._2)
      case scala.util.Failure(f) ⇒ throw f
    }
    res.map(_.asString).toSet
  }

  @Test
  def single_public_class() = {
    convert("X.java" → """
      public class X {}
    """) === Set("X")
  }

  @Test
  def multiple_classes() = {
    convert("X.java" → """
      public class X {}
      class Y {}
      class Z {}
    """) === Set("X", "Y", "Z")
  }

  @Test
  def single_field() = {
    convert("X.java" → """
      public class X {
        int i;
      }
    """) === Set("X", "X.i")
  }

  @Test
  def multiple_fields() = {
    convert("X.java" → """
      public class X {
        int i;
        int j;
        int k;
      }
    """) === Set("X", "X.i", "X.j", "X.k")
  }

  @Test
  def multiple_fields_in_multiple_classes() = {
    convert("X.java" → """
      public class X {
        int i;
        int j;
      }
      class Y {
        int k;
        int l;
      }
    """) === Set("X", "X.i", "X.j", "Y", "Y.k", "Y.l")
  }

  @Test
  def single_method() = {
    convert("X.java" → """
      public class X {
        int i() {
          return 0;
        }
      }
    """) === Set("X", "X.i()I")
  }

  @Test
  def multiple_methods() = {
    convert("X.java" → """
      public class X {
        int i() {
          return 0;
        }
        int j() {
          return 0;
        }
      }
    """) === Set("X", "X.i()I", "X.j()I")
  }

  @Test
  def multiple_methods_in_multiple_classes() = {
    convert("X.java" → """
      public class X {
        int i() {
          return 0;
        }
      }
      class Y {
        int j() {
          return 0;
        }
      }
    """) === Set("X", "X.i()I", "Y", "Y.j()I")
  }

  @Test
  def method_with_parameter() = {
    convert("X.java" → """
      public class X {
        int f(int i) {
          return 0;
        }
      }
    """) === Set("X", "X.f(I)I", "X.f(I)I.<param>i")
  }

  @Test
  def overloaded_method() = {
    convert("X.java" → """
      public class X {
        int f(int i) {
          return 0;
        }
        int f(int i, int j) {
          return 0;
        }
      }
    """) === Set("X", "X.f(I)I", "X.f(I)I.<param>i", "X.f(II)I", "X.f(II)I.<param>i", "X.f(II)I.<param>j")
  }

  @Test
  def packages() = {
    convert("a/b/c/X.java" → """
      package a.b.c;
      public class X {}
    """) === Set("a.b.c.X")
  }
}
