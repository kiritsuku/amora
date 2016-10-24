package amora.converter

import amora.backend.services.DottySourceIndexer
import amora.backend.IgnoreLogger
import org.junit.Test

class DotcConverterTest extends ScalaCompilerTest {

  import amora.TestUtils._

  def convert(data: (String, String)*): Set[String] = {
    val indexer = new DottySourceIndexer(IgnoreLogger)
    val res = indexer.convertToSchema(data).flatMap(_._2)
    res.map(_.asString).toSet
  }
}
