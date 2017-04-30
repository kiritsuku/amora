package amora.backend.indexer

import org.junit.Test

class CommitTest extends RestApiTest {
  import amora.TestUtils._

  @Test
  def return_empty_string_when_there_is_no_commit_yet() = {
    headCommit() === ""
  }
}
