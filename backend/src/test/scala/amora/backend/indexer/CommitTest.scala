package amora.backend.indexer

import org.junit.Test

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes
import amora.backend.CustomContentTypes
import amora.backend.schema.Schema

class CommitTest extends RestApiTest {
  import amora.TestUtils._

  case class Person(name: String, age: Int)

  def buildTurtleUpdate(ps: Seq[Person]) = Schema.turtleBuilder {
    (addPrefix, addData) ⇒

    addPrefix("Person", "http://amora.center/kb/amora/Schema/Person/")
    addPrefix("PersonData", "http://amora.center/kb/amora/Person/")

    for (p ← ps) {
      val id = s"PersonData:${p.name}"
      addData(id, "a", "Person:")
      addData(id, "Person:name", s""""${p.name}"""")
      addData(id, "Person:age", p.age.toString)
    }
  }

  @Test
  def return_empty_string_when_there_is_no_commit_yet() = {
    headCommit() === ""
  }

  @Test
  def head_commit_exists_for_single_update() = {
    val e = HttpEntity(CustomContentTypes.`text/turtle(UTF-8)`, buildTurtleUpdate(Seq(Person("franz", 49))))
    testReq(post("http://amora.center/turtle-update", e)) {
      status === StatusCodes.OK
    }
    headCommit().take(8) === "e002e422"
  }

  @Test
  def list_no_commits_when_there_are_no_commits_yet() = {
    testReq(get("http://amora.center/commit/list")) {
      checkStatus()
      respAsString === ""
    }
  }
}
