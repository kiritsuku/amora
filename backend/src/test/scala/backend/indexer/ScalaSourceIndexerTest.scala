package backend
package indexer

import org.junit.Test

import backend.actors.IndexerMessage

class ScalaSourceIndexerTest {

  import backend.TestUtils._

  case class Data(varName: String, value: String)

  def ask(modelName: String, data: Seq[(String, String)], query: String): Seq[Data] = {
    val indexer = new Indexer(modelName)
    val dataset = indexer.mkInMemoryDataset
    val res = indexer.writeDataset(dataset) { dataset ⇒
      indexer.withModel(dataset) { model ⇒
        val sindexer = new ScalaSourceIndexer(IgnoreLogger)
        sindexer.convertToHierarchy(data) match {
          case scala.util.Success(data) ⇒
            data foreach {
              case (filename, data) ⇒
                indexer.addFile(IndexerMessage.File(IndexerMessage.NoOrigin, filename, data))(model).get
            }
          case scala.util.Failure(f) ⇒
            throw f
        }

        if (debugTests) {
          println(indexer.queryResultAsString("select * { ?s ?p ?o }", model))
          println(indexer.queryResultAsString(query, model))
        }
        indexer.flattenedQueryResult(query, model) { (v, q) ⇒
          val res = q.get(v)
          require(res != null, s"The variable `$v` does not exist in the result set.")
          Data(v, res.toString)
        }
      }
    }

    res.sortBy(d ⇒ (d.varName, d.value))
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
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "class"] s:name ?name .
        }
      """) === Seq(
        Data("name", "C1"),
        Data("name", "C2"),
        Data("name", "C3"))
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
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "class"] c:owner [c:name "f1.scala"]; s:name ?name .
        }
      """) === Seq(
        Data("name", "C1"),
        Data("name", "C2"))
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
        SELECT ?name WHERE {
          ?class c:attachment "class" .
          ?class s:name ?className .
          FILTER (str(?className) = "Y") .
          ?ref c:reference ?class .
          ?ref c:owner ?owner .
          ?owner s:name ?name .
        }
      """) === Seq(
        Data("name", "f"),
        Data("name", "m"))
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
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "package"] s:name ?name .
        }
      """) === Seq(
        Data("name", "a"),
        Data("name", "b"),
        Data("name", "c"))
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
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "val"] s:name ?name .
        }
      """) === Seq(
        Data("name", "b"),
        Data("name", "e"))
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
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "lazy", "val"] s:name ?name .
        }
      """) === Seq(
        Data("name", "e"))
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
        PREFIX s:<http://schema.org/>
        SELECT ?name WHERE {
          ?s c:attachment "val"; s:name ?name .
          FILTER NOT EXISTS {
            ?s c:attachment "lazy" .
          }
        }
      """) === Seq(
        Data("name", "b"))
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
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "var"] s:name ?name .
        }
      """) === Seq(
        Data("name", "d"))
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
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "class"] s:name ?name .
        }
      """) === Seq(
        Data("name", "A"),
        Data("name", "B"))
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
        PREFIX s:<http://schema.org/>
        SELECT ?name WHERE {
          ?s c:attachment "class"; s:name ?name .
          FILTER NOT EXISTS {
            ?s c:attachment "abstract" .
          }
        }
      """) === Seq(
        Data("name", "A"))
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
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "trait"] s:name ?name .
        }
      """) === Seq(
        Data("name", "C"))
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
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "object"] s:name ?name .
        }
      """) === Seq(
        Data("name", "D"))
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
