package indexer

import org.junit.Test

import indexer.hierarchy.Hierarchy
import plugin.TestUtils

class IndexerTest {

  import TestUtils._

  case class Data(varName: String, value: String)

  def ask(data: Seq[(String, Seq[Hierarchy])], query: String)(implicit modelName: String): Seq[Data] = {
    Indexer.withInMemoryDataset { dataset ⇒
      Indexer.withModel(dataset, modelName) { model ⇒
        data foreach {
          case (filename, data) ⇒
            Indexer.add(modelName, filename, data)(model)
        }
        val res = Indexer.queryResult(modelName, query, model) { (v, q) ⇒
          Data(v, q.get(v).toString)
        }
        return res.sortBy(d ⇒ (d.varName, d.value))
      }
    }
    throw new RuntimeException("An error happened during the test. See previous error message.")
  }

  @Test
  def find_top_level_classes() = {
    implicit val modelName = "http://test.model/"
    val filename = "<memory>"
    val data = convertToHierarchy(filename → """
      package a.b.c
      class C1
      class C2
      class C3
    """)

    val result = ask(data, s"""
      PREFIX c:<$modelName>
      SELECT * WHERE {
        ?class c:tpe "class" .
      }
    """)
    result === Seq(
        Data("class", s"${modelName}_root_/a/b/c/C1"),
        Data("class", s"${modelName}_root_/a/b/c/C2"),
        Data("class", s"${modelName}_root_/a/b/c/C3")
    )
  }

  @Test
  def find_methods_in_top_level_classes() = {
    implicit val modelName = "http://test.model/"
    val filename = "<memory>"
    val data = convertToHierarchy(filename → """
      package a.b.c
      class C1 {
        def m1 = 0
      }
      class C2 {
        def m2 = 0
      }
      class C3 {
        def m3 = 0
      }
    """)

    val result = ask(data, s"""
      PREFIX c:<$modelName>
      SELECT * WHERE {
        ?member c:tpe "member" .
      }
    """)
    result === Seq(
        Data("member", s"${modelName}_root_/a/b/c/C1/m1"),
        Data("member", s"${modelName}_root_/a/b/c/C2/m2"),
        Data("member", s"${modelName}_root_/a/b/c/C3/m3")
    )
  }

  @Test
  def find_all_methods_of_single_class() = {
    implicit val modelName = "http://test.model/"
    val filename = "<memory>"
    val data = convertToHierarchy(filename → """
      package a.b.c
      class C1 {
        def m11 = 0
        def m12 = 0
      }
      class C2 {
        def m2 = 0
      }
      class C3 {
        def m3 = 0
      }
    """)

    val result = ask(data, s"""
      PREFIX c:<$modelName>
      PREFIX s:<http://schema.org/>
      SELECT ?member WHERE {
        ?class c:tpe "class" .
        ?class s:name ?className .
        FILTER (str(?className) = "C1") .
        ?member c:parent ?class .
      }
    """)
    result === Seq(
        Data("member", s"${modelName}_root_/a/b/c/C1/m11"),
        Data("member", s"${modelName}_root_/a/b/c/C1/m12")
    )
  }

  @Test
  def find_classes_of_single_file() = {
    implicit val modelName = "http://test.model/"
    val data = convertToHierarchy(
    "f1.scala" → """
      package a.b.c
      class C1
      class C2
    """,
    "f2.scala" → """
      package d.e.f
      class D1
      class D2
    """)

    val result = ask(data, s"""
      PREFIX c:<$modelName>
      PREFIX s:<http://schema.org/>
      SELECT ?class WHERE {
        ?class c:tpe "class" .
        ?class c:file "f1.scala" .
      }
    """)
    result === Seq(
        Data("class", s"${modelName}_root_/a/b/c/C1"),
        Data("class", s"${modelName}_root_/a/b/c/C2")
    )
  }
}
