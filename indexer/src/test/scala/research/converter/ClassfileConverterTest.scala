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
}
