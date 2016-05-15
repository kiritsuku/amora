package backend
package indexer

import scala.util._

import org.junit.Test
import backend.TestUtils

class ScalaSourceIndexerTest {

  import TestUtils._

  case class Data(varName: String, value: String)

  def ask(modelName: String, data: Seq[(String, String)], query: String): Seq[Data] = {
    val res = Indexer.withInMemoryDataset { dataset ⇒
      Indexer.withModel(dataset, modelName) { model ⇒
        val indexer = new ScalaSourceIndexer(IgnoreLogger)
        indexer.convertToHierarchy(data) match {
          case scala.util.Success(data) ⇒
            data foreach {
              case (filename, data) ⇒
                Indexer.addFile(modelName, filename, data)(model)
            }
          case scala.util.Failure(f) ⇒
            throw f
        }

        if (debugTests) {
          Indexer.queryResultAsString(modelName, "select * { ?s ?p ?o }", model) foreach println
          Indexer.queryResultAsString(modelName, query, model) foreach println
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
    ask(modelName, Seq(
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
    ask(modelName, Seq(
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
        PREFIX s:<http://schema.org/>
        SELECT ?name WHERE {
          [c:attachment "def"] s:name ?name .
        }
      """) === Seq(
        Data("name", "m1"),
        Data("name", "m2"),
        Data("name", "m3"))
  }

  @Test
  def find_all_methods_of_single_class() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
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
        SELECT ?name WHERE {
          ?class c:attachment "class" ; s:name "C1" .
          [c:owner ?class] s:name ?name .
        }
      """) === Seq(
        Data("name", "m11"),
        Data("name", "m12"))
  }

  @Test
  def find_classes_of_single_file() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
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
    ask(modelName, Seq(
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
        SELECT ?owner WHERE {
          ?class c:attachment "class" .
          ?class s:name ?className .
          FILTER (str(?className) = "Y") .
          ?ref c:reference ?class .
          ?ref c:owner ?owner .
        }
      """) === Seq(
        Data("owner", s"${modelName}_root_/a/b/c/X/m%28%29Ld%2Fe%2Ff%2FY%3B"),
        Data("owner", s"${modelName}_root_/d/e/f"))
  }

  @Test
  def find_package_declarations() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
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
    ask(modelName, Seq(
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
    ask(modelName, Seq(
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
    ask(modelName, Seq(
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
    ask(modelName, Seq(
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
    ask(modelName, Seq(
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
        PREFIX s:<http://schema.org/>
        SELECT ?name WHERE {
          [c:attachment "def"] s:name ?name .
        }
      """) === Seq(
        Data("name", "a"),
        Data("name", "c"))
    }

  @Test
  def find_classes() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
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
    ask(modelName, Seq(
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
    ask(modelName, Seq(
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
    ask(modelName, Seq(
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

  @Test
  def find_private_class_parameters() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
      "<memory>" → """
        class X(i: Int, j: String)
      """), s"""
        PREFIX c:<$modelName>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "param"] s:name ?name .
        }
      """) === Seq(
        Data("name", "i"),
        Data("name", "j"))
    }

  @Test
  def find_public_class_parameters() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
      "<memory>" → """
        class X(val i: Int, val j: String) {
          val k = 0
        }
      """), s"""
        PREFIX c:<$modelName>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "param"] s:name ?name .
        }
      """) === Seq(
        Data("name", "i"),
        Data("name", "j"))
    }

  @Test
  def public_class_parameters_are_vals() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
      "<memory>" → """
        class X(val i: Int, val j: String) {
          val k = 0
        }
      """), s"""
        PREFIX c:<$modelName>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "val"] s:name ?name .
        }
      """) === Seq(
        Data("name", "i"),
        Data("name", "j"),
        Data("name", "k"))
    }

  @Test
  def private_class_parameters_are_vals() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
      "<memory>" → """
        class X(i: Int, j: String) {
          val k = 0
        }
      """), s"""
        PREFIX c:<$modelName>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "val"] s:name ?name .
        }
      """) === Seq(
        Data("name", "i"),
        Data("name", "j"),
        Data("name", "k"))
    }

  @Test
  def class_parameters_can_be_vars() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
      "<memory>" → """
        class X(val i: Int, var j: String) {
          val k = 0
        }
      """), s"""
        PREFIX c:<$modelName>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "var"] s:name ?name .
        }
      """) === Seq(
        Data("name", "j"))
    }

  @Test
  def method_parameters_are_vals() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
      "<memory>" → """
        class X {
          def f(i: Int, j: String) = 0
        }
      """), s"""
        PREFIX c:<$modelName>
        PREFIX s:<http://schema.org/>
        SELECT ?name WHERE {
          [c:attachment "val"] s:name ?name .
        }
      """) === Seq(
        Data("name", "i"),
        Data("name", "j"))
    }

  @Test
  def method_parameters() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
      "<memory>" → """
        class X {
          def f(i: Int, j: String) = 0
        }
      """), s"""
        PREFIX c:<$modelName>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "param"] s:name ?name .
        }
      """) === Seq(
        Data("name", "i"),
        Data("name", "j"))
  }

  @Test
  def owner_of_refs_in_if_expr() = {
    val modelName = "http://test.model/"
    ask(modelName, Seq(
      "<memory>" → """
        class X {
          val b1 = true
          val b2 = true
          val b3 = true
          def f = if (b1) b2 else b3
        }
      """), s"""
        PREFIX c:<$modelName>
        PREFIX s:<http://schema.org/>
        SELECT ?name WHERE {
          ?def s:name "f" .
          [c:owner ?def] s:name ?name .
        }
      """) === Seq("Boolean", "b1", "b2", "b3").map(Data("name", _))
  }
}
