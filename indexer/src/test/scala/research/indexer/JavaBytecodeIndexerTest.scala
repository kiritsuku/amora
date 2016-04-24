package research
package indexer

import scala.util._
import org.junit.Test

class JavaBytecodeIndexerTest {

  import TestUtils._

  case class Data(varName: String, value: String)

  def ask(modelName: String, rawQuery: String, data: (String, String)*): Seq[Data] = {
    val query = rawQuery.replaceFirst("""\?MODEL\?""", modelName)
    val res = Indexer.withInMemoryDataset { dataset ⇒
      Indexer.withModel(dataset, modelName) { model ⇒
        bytecodeToHierarchy(data) match {
          case Success(data) ⇒
            data foreach {
              case (filename, data) ⇒
                Indexer.add(modelName, filename, data)(model)
            }
          case Failure(f) ⇒
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
}
