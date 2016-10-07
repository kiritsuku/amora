package amora.converter

import amora.backend.IgnoreLogger
import amora.backend.services.ScalaSourceIndexer

class ScalacConverterTest extends ScalaCompilerTest {

  def convert(data: (String, String)*): Set[String] = {
    val indexer = new ScalaSourceIndexer(IgnoreLogger)
    val res = indexer.convertToHierarchy(data).flatMap(_._2)
    res.map(_.asString).toSet
  }
}
