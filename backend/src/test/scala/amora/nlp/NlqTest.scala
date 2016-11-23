package amora.nlp

import org.junit.Test

import amora.backend.indexer.RestApiTest
import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

class NlqTest extends RestApiTest {
  import amora.TestUtils._

  @Test
  def list_classes(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "x.scala" → """
        class A
        class B
        class C
      """)
    nlqRequest("list classes") === Seq(
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
    nlqRequest("list traits") === Seq(
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
    nlqRequest("list names of classes") === Seq(
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
    nlqRequest("list names of methods").sorted === Seq(
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
    nlqRequest("list names of defs").sorted === Seq(
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
    nlqRequest("list class A") === Seq(
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
    nlqRequest("list name of class A").sorted === Seq(
      "A"
    )
  }

}
