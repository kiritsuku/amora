package amora.backend.indexer

import org.junit.Test

import amora.backend.schema.Schema
import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

class MultiProjectTest extends RestApiTest {
  import amora.TestUtils._

  @Test
  def add_multiple_projects(): Unit = {
    val p1 = Project("p1")
    val p2 = Project("p2")
    turtleRequest(Schema.mkTurtleString(Seq(p1, p2)))
    sparqlRequest("""
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      select * where {
        [a p:] p:name ?name .
      }
    """) === Seq(
        Seq(Data("name", "p1")),
        Seq(Data("name", "p2")))
  }

  @Test
  def add_multiple_projects_with_artifacts(): Unit = {
    val a1 = Artifact(Project("p1"), "o1", "n1", "v1")
    val a2 = Artifact(Project("p2"), "o2", "n2", "v2")
    turtleRequest(Schema.mkTurtleString(Seq(a1, a2)))
    sparqlRequest("""
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      select * where {
        [a a:] a:name ?name .
      }
    """) === Seq(
        Seq(Data("name", "n1")),
        Seq(Data("name", "n2")))
  }
}
