package backend.indexer

import scala.util.Failure
import scala.util.Success

import org.junit.Test

import backend.TestUtils
import backend.actors.IndexerMessage._

class ModelIndexerTest {
  import TestUtils._

  case class Data(varName: String, value: String)

  def ask(modelName: String, rawQuery: String, artifact: Artifact): Seq[Data] = {
    val query = rawQuery.replaceFirst("""\?MODEL\?""", modelName)
    val res = Indexer.withInMemoryDataset { dataset ⇒
      Indexer.withModel(dataset, modelName) { model ⇒
        Indexer.addArtifact(modelName, artifact)(model).get

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
  def find_single_project() = {
    val project = Project("project")
    val artifact = Artifact(project, "organization", "name", "v1")

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT * WHERE {
          [a c:Project] c:name ?name .
        }
      """, artifact) === Seq(Data("name", "project"))
  }
}
