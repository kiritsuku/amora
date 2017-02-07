package amora.nlp

import org.junit.Test

import amora.backend.indexer.RestApiTest
import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

class NlqTest extends RestApiTest {
  import amora.TestUtils._

  private def fmt(str: String): String = {
    val lines = str.split("\n")
    val indent = lines.dropWhile(_.isEmpty()).head.takeWhile(_.isWhitespace).size
    lines.map(_.drop(indent)).mkString("\n").trim + "\n"
  }

  @Test
  def list_classes(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A
        class B
        class C
      """)
    nlqRequest("list classes").sortedAsList === Seq(
      "http://amora.center/kb/amora/Class/p/o/n/v1/A",
      "http://amora.center/kb/amora/Class/p/o/n/v1/B",
      "http://amora.center/kb/amora/Class/p/o/n/v1/C"
    )
  }

  @Test
  def list_traits(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        trait A
        class B
        trait C
      """)
    nlqRequest("list traits").sortedAsList === Seq(
      "http://amora.center/kb/amora/Trait/p/o/n/v1/A",
      "http://amora.center/kb/amora/Trait/p/o/n/v1/C"
    )
  }

  @Test
  def list_names_of_classes(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A
        class B
        class C
      """)
    nlqRequest("list names of classes").sortedAsList === Seq(
      "A",
      "B",
      "C"
    )
  }

  @Test
  def list_names_of_methods(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          def a = 0
        }
        class B {
          def b1 = 0
          def b2 = 0
        }
      """)
    nlqRequest("list names of methods").sortedAsList === Seq(
      "a",
      "b1",
      "b2",
      "this",
      "this"
    )
  }

  @Test
  def list_names_of_defs(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          def a = 0
        }
        class B {
          def b1 = 0
          def b2 = 0
        }
      """)
    nlqRequest("list names of defs").sortedAsList === Seq(
      "a",
      "b1",
      "b2",
      "this",
      "this"
    )
  }

  @Test
  def list_given_class(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          def a = 0
        }
        class B {
          def b1 = 0
          def b2 = 0
        }
      """)
    nlqRequest("list class A").sortedAsList === Seq(
      "http://amora.center/kb/amora/Class/p/o/n/v1/A"
    )
  }

  @Test
  def list_name_of_class(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          def a = 0
        }
        class B {
          def b1 = 0
          def b2 = 0
        }
      """)
    nlqRequest("list name of class A").sortedAsList === Seq(
      "A"
    )
  }

  @Test
  def list_names_of_methods_of_given_class(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          def a = 0
        }
        class B {
          def b1 = 0
          def b2 = 0
        }
      """)
    nlqRequest("list names of methods of class B").sortedAsList === Seq(
      "b1",
      "b2",
      "this"
    )
  }

  @Test
  def list_names_of_nested_methods_of_given_method(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          def a = {
            def n1 = 0
            def n2 = 0
            n1 + n2
          }
          def b = 0
        }
      """)
    nlqRequest("list names of methods of method a").sortedAsList === Seq(
      "n1",
      "n2"
    )
  }

  @Test
  def list_declarations(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          val a = 0
          var b = 0
          def c = 0
          lazy val d = 0
        }
      """)
    nlqRequest("list names of declarations").sortedAsList === Seq(
      "A",
      "a",
      "b",
      "c",
      "d",
      "this"
    )
  }

  @Test
  def list_decls(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          val a = 0
          var b = 0
          def c = 0
          lazy val d = 0
        }
      """)
    nlqRequest("list names of decls").sortedAsList === Seq(
      "A",
      "a",
      "b",
      "c",
      "d",
      "this"
    )
  }

  @Test
  def show_decls(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          val a = 0
          var b = 0
          def c = 0
          lazy val d = 0
        }
      """)
    nlqRequest("show names of decls").sortedAsList === Seq(
      "A",
      "a",
      "b",
      "c",
      "d",
      "this"
    )
  }

  @Test
  def show_decls_as_list(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A {
          val a = 0
          var b = 0
          def c = 0
          lazy val d = 0
        }
      """)
    nlqRequest("show names of decls as list").renderAsString === fmt("""
      - A
      - a
      - b
      - c
      - d
      - this
    """)
  }
    )
  }
}
