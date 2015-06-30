package tutorial.webapp

import utest._

import org.scalajs.jquery.jQuery

object TutorialTest extends TestSuite {

  // Initialize App
  TutorialApp.setupUI()

  override def tests = TestSuite {
    'HelloWorld {
      assert(jQuery("p:contains('Hello World')").length == 1)
    }
  }
}
