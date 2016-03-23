package indexer

import scala.util._

import org.junit.Test

import indexer.hierarchy.Hierarchy
import plugin.TestUtils

class IndexerTest {

  import TestUtils._

  case class Data(varName: String, value: String)

  def ask(modelName: String, data: Seq[(String, Seq[Hierarchy])], query: String): Seq[Data] = {
    val res = Indexer.withInMemoryDataset { dataset ⇒
      Indexer.withModel(dataset, modelName) { model ⇒
        data foreach {
          case (filename, data) ⇒
            Indexer.add(modelName, filename, data)(model)
        }
        Indexer.queryResult(modelName, query, model) { (v, q) ⇒
          val res = q.get(v)
          require(res != null, s"The variable `$v` does not exist in the result set.")
          Data(v, res.toString)
        }
      }.flatten
    }.flatten

    res match {
      case Success(res) ⇒
        res.sortBy(d ⇒ (d.varName, d.value))
      case Failure(f) ⇒
        throw new RuntimeException("An error happened during the test.", f)
    }
  }

  @Test
  def find_top_level_classes() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
        package a.b.c
        class C1
        class C2
        class C3
      """), s"""
        PREFIX c:<$modelName>
        SELECT * WHERE {
          ?class c:attachment "class" .
        }
      """) === Seq(
        Data("class", s"${modelName}_root_/a/b/c/C1"),
        Data("class", s"${modelName}_root_/a/b/c/C2"),
        Data("class", s"${modelName}_root_/a/b/c/C3"))
  }

  @Test
  def find_methods_in_top_level_classes() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
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
      """), s"""
        PREFIX c:<$modelName>
        SELECT * WHERE {
          ?def c:attachment "def" .
        }
      """) === Seq(
        Data("def", s"${modelName}_root_/a/b/c/C1/m1"),
        Data("def", s"${modelName}_root_/a/b/c/C2/m2"),
        Data("def", s"${modelName}_root_/a/b/c/C3/m3"))
  }

  @Test
  def find_all_methods_of_single_class() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
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
      """), s"""
        PREFIX c:<$modelName>
        PREFIX s:<http://schema.org/>
        SELECT ?member WHERE {
          ?class c:attachment "class" .
          ?class s:name ?className .
          FILTER (str(?className) = "C1") .
          ?member c:parent ?class .
        }
      """) === Seq(
        Data("member", s"${modelName}_root_/a/b/c/C1/m11"),
        Data("member", s"${modelName}_root_/a/b/c/C1/m12"))
  }

  @Test
  def find_classes_of_single_file() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "f1.scala" → """
        package a.b.c
        class C1
        class C2
      """,
      "f2.scala" → """
        package d.e.f
        class D1
        class D2
      """), s"""
        PREFIX c:<$modelName>
        SELECT ?class WHERE {
          ?class c:attachment "class" .
          ?class c:file "f1.scala" .
        }
      """) === Seq(
        Data("class", s"${modelName}_root_/a/b/c/C1"),
        Data("class", s"${modelName}_root_/a/b/c/C2"))
  }

  @Test
  def find_usages() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "f1.scala" → """
        package a.b.c
        import d.e.f.Y
        class X {
          def m: Y = null
        }
      """,
      "f2.scala" → """
        package d.e.f
        class Y
      """), s"""
        PREFIX c:<$modelName>
        PREFIX s:<http://schema.org/>
        SELECT ?usage WHERE {
          ?class c:attachment "class" .
          ?class s:name ?className .
          FILTER (str(?className) = "Y") .
          ?ref c:reference ?class .
          ?ref c:usage ?usage .
        }
      """) === Seq(
        Data("usage", s"${modelName}_root_/a/b/c/X/m"),
        Data("usage", s"${modelName}_root_/d/e/f"))
  }

  @Test
  def find_package_declarations() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
        package a.b.c
        class X
      """), s"""
        PREFIX c:<$modelName>
        SELECT ?s WHERE {
          ?s c:attachment "package" .
        }
      """) === Seq(
        Data("s", s"${modelName}_root_/a"),
        Data("s", s"${modelName}_root_/a/b"),
        Data("s", s"${modelName}_root_/a/b/c"))
    }

  @Test
  def find_vals_and_lazy_vals() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
        package pkg
        class X {
          def a = 0
          val b = 0
          def c = 0
          var d = 0
          lazy val e = 0
        }
      """), s"""
        PREFIX c:<$modelName>
        SELECT ?s WHERE {
          ?s c:attachment "val" .
        }
      """) === Seq(
        Data("s", s"${modelName}_root_/pkg/X/b"),
        Data("s", s"${modelName}_root_/pkg/X/e"))
    }

  @Test
  def find_lazy_vals() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
        package pkg
        class X {
          def a = 0
          val b = 0
          def c = 0
          var d = 0
          lazy val e = 0
        }
      """), s"""
        PREFIX c:<$modelName>
        SELECT ?s WHERE {
          ?s c:attachment "lazy", "val" .
        }
      """) === Seq(
        Data("s", s"${modelName}_root_/pkg/X/e"))
    }

  @Test
  def find_vals() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
        package pkg
        class X {
          def a = 0
          val b = 0
          def c = 0
          var d = 0
          lazy val e = 0
        }
      """), s"""
        PREFIX c:<$modelName>
        SELECT ?s WHERE {
          ?s c:attachment "val" .
          FILTER NOT EXISTS {
            ?s c:attachment "lazy" .
          }
        }
      """) === Seq(
        Data("s", s"${modelName}_root_/pkg/X/b"))
    }

  @Test
  def find_vars() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
        package pkg
        class X {
          def a = 0
          val b = 0
          def c = 0
          var d = 0
          lazy val e = 0
        }
      """), s"""
        PREFIX c:<$modelName>
        SELECT ?s WHERE {
          ?s c:attachment "var" .
        }
      """) === Seq(
        Data("s", s"${modelName}_root_/pkg/X/d"))
    }

  @Test
  def find_methods() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
        package pkg
        class X {
          def a = 0
          val b = 0
          def c = 0
          var d = 0
          lazy val e = 0
        }
      """), s"""
        PREFIX c:<$modelName>
        SELECT ?s WHERE {
          ?s c:attachment "def" .
        }
      """) === Seq(
        Data("s", s"${modelName}_root_/pkg/X/a"),
        Data("s", s"${modelName}_root_/pkg/X/c"))
    }

  @Test
  def find_classes() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
        package pkg
        class A
        abstract class B
        trait C
        object D
      """), s"""
        PREFIX c:<$modelName>
        SELECT ?s WHERE {
          ?s c:attachment "class" .
        }
      """) === Seq(
        Data("s", s"${modelName}_root_/pkg/A"),
        Data("s", s"${modelName}_root_/pkg/B"))
    }

  @Test
  def find_classes_but_not_abstract_ones() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
        package pkg
        class A
        abstract class B
        trait C
        object D
      """), s"""
        PREFIX c:<$modelName>
        SELECT ?s WHERE {
          ?s c:attachment "class" .
          FILTER NOT EXISTS {
            ?s c:attachment "abstract" .
          }
        }
      """) === Seq(
        Data("s", s"${modelName}_root_/pkg/A"))
    }

  @Test
  def find_traits() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
        package pkg
        class A
        abstract class B
        trait C
        object D
      """), s"""
        PREFIX c:<$modelName>
        SELECT ?s WHERE {
          ?s c:attachment "trait" .
        }
      """) === Seq(
        Data("s", s"${modelName}_root_/pkg/C"))
    }

  @Test
  def find_objects() = {
    val modelName = "http://test.model/"
    ask(modelName, convertToHierarchy(
      "<memory>" → """
        package pkg
        class A
        abstract class B
        trait C
        object D
      """), s"""
        PREFIX c:<$modelName>
        SELECT ?s WHERE {
          ?s c:attachment "object" .
        }
      """) === Seq(
        Data("s", s"${modelName}_root_/pkg/D"))
    }
}
