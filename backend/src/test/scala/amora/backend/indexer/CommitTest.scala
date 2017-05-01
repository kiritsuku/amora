package amora.backend.indexer

import org.junit.Test

import amora.backend.schema.Schema
import amora.api._

class CommitTest extends RestApiTest {
  import amora.TestUtils._

  private case class Person(name: String, age: Int)

  private def buildTurtleUpdate(ps: Seq[Person]) = Schema.turtleBuilder {
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

  def update1() = turtleRequest(buildTurtleUpdate(Seq(Person("franz", 49))))
  def update2() = turtleRequest(buildTurtleUpdate(Seq(Person("hugo", 25))))
  def update3() = turtleRequest(buildTurtleUpdate(Seq(Person("sarah", 27))))

  @Test
  def return_empty_string_when_there_is_no_commit_yet() = {
    headCommit() === ""
  }

  @Test
  def head_commit_exists_for_single_update() = {
    update1()
    headCommit().take(8) === "e002e422"
  }

  @Test
  def list_no_commits_when_there_are_no_commits_yet() = {
    listCommits() === ""
  }

  @Test
  def list_single_commit() = {
    update1()
    listCommits().take(8) === "e002e422"
  }

  @Test
  def head_commit_exists_after_multiple_updates() = {
    update1()
    update2()
    update3()
    headCommit().take(8) === "946c1b6a"
  }

  @Test
  def list_commits_after_multiple_updates() = {
    update1()
    update2()
    update3()
    listCommits().split(",").map(_.take(8)).toSeq === Seq(
        "946c1b6a",
        "265fb068",
        "e002e422")
  }

  @Test
  def get_data_of_commit() = {
    update1()
    update2()
    update3()

    val Array(hash1, hash2, hash3) = listCommits().split(",")
    val q = sparqlQuery"""
      prefix Person:<http://amora.center/kb/amora/Schema/Person/>
      select * where {
        [Person:name ?name; Person:age ?age] .
      }
      order by ?name
    """

    modelAsData(showCommit(hash1), q) === Seq(
        Seq(Data("name", "sarah"), Data("age", "27")))
    modelAsData(showCommit(hash2), q) === Seq(
        Seq(Data("name", "hugo"), Data("age", "25")))
    modelAsData(showCommit(hash3), q) === Seq(
        Seq(Data("name", "franz"), Data("age", "49")))

    sparqlRequest(q) === Seq(
        Seq(Data("name", "franz"), Data("age", "49")),
        Seq(Data("name", "hugo"), Data("age", "25")),
        Seq(Data("name", "sarah"), Data("age", "27")))
  }
}
