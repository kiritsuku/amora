package amora.backend.indexer

import org.junit.Test

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes
import amora.backend.CustomContentTypes

class CommitTest extends RestApiTest {
  import amora.TestUtils._

  @Test
  def return_empty_string_when_there_is_no_commit_yet() = {
    headCommit() === ""
  }

  @Test
  def head_commit_exists_for_single_update() = {
    val e = HttpEntity(CustomContentTypes.`text/turtle(UTF-8)`, """
      @prefix Person:      <http://amora.center/kb/amora/Schema/Person/>
      @prefix PersonData:  <http://amora.center/kb/amora/Person/>

      PersonData:franz
        a                  Person: ;
        Person:name        "franz" ;
        Person:age         49 ;
      .
    """)
    testReq(post("http://amora.center/turtle-update", e)) {
      status === StatusCodes.OK
    }
    headCommit().take(8) === "f734d1fb"
  }
}
