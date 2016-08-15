package backend
package indexer

import scala.util._

import org.junit.Test

import backend.actors.IndexerMessage

class JavaBytecodeIndexerTest {

  import backend.TestUtils._

  case class Data(varName: String, value: String)

  def ask(modelName: String, rawQuery: String, data: (String, String)*): Seq[Data] = {
    val indexer = new Indexer(modelName)
    val dataset = indexer.mkInMemoryDataset
    val query = rawQuery.replaceFirst("""\?MODEL\?""", modelName)
    val res = indexer.writeDataset(dataset) { dataset ⇒
      indexer.withModel(dataset) { model ⇒
        val jindexer = new JavaBytecodeIndexer(IgnoreLogger)
        jindexer.bytecodeToHierarchy(data) match {
          case Success(data) ⇒
            data foreach {
              case (filename, data) ⇒
                indexer.addFile(IndexerMessage.File(IndexerMessage.NoOrigin, filename, data))(model)
            }
          case Failure(f) ⇒
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

  def modelName = "http://test.model/"

  @Test
  def classes() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "class"] s:name ?name .
        }
      """,
      "X.java" → """
        public class X {}
      """) === Seq(Data("name", "X"))
  }

  @Test
  def methods() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "def"] s:name ?name .
        }
      """,
      "X.java" → """
        public class X {
          int i() {
            return 0;
          }
          int j(int i) {
            return 0;
          }
        }
      """) === Seq(Data("name", "i"), Data("name", "j"))
  }

  @Test
  def fields() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "var"] s:name ?name .
        }
      """,
      "X.java" → """
        public class X {
          int i = 0;
          int j = 0;
        }
      """) === Seq(Data("name", "i"), Data("name", "j"))
  }

  @Test
  def index_method_parameter_as_vars() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "var"] s:name ?name .
        }
      """,
      "X.java" → """
        public class X {
          void f(int i, int j) {}
        }
      """) === Seq(Data("name", "i"), Data("name", "j"))
  }

  @Test
  def index_method_parameter_as_params() = {
    ask(modelName, s"""
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:attachment "param"] s:name ?name .
        }
      """,
      "X.java" → """
        public class X {
          void f(int i, int j) {}
        }
      """) === Seq(Data("name", "i"), Data("name", "j"))
  }
}
