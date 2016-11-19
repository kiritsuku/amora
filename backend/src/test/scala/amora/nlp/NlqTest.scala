package amora.nlp

import org.junit.Test

import amora.backend.indexer.RestApiTest

class NlqTest extends RestApiTest {
  import amora.TestUtils._

  @Test
  def xxx(): Unit = {
    nlqRequest("list methods of classes") === Seq()
  }
}
