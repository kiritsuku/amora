package research
package converter

import org.junit.Test

import indexer.hierarchy.Root

class ClassfileConverterTest {

  import TestUtils._

  def convert(src: String): Set[String] =
    convert("<memory>" → src)

  def convert(data: (String, String)*): Set[String] = {
    val res = bytecodeToHierarchy(data: _*).flatMap(_._2)
    res.map(_.asString).map(_.drop(Root.name.length+1)).toSet
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
    """) === Set("X", "X.i")
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
    """) === Set("X", "X.i", "X.j")
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
    """) === Set("X", "X.i", "Y", "Y.j")
  }
}
