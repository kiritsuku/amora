package amora.converter

import amora.backend.IgnoreLogger
import amora.backend.indexer.ScalaSourceIndexer

class ScalacConverterTest extends ScalaCompilerTest {

  def convert(data: (String, String)*): Set[String] = {
    val indexer = new ScalaSourceIndexer(IgnoreLogger)
    val res = indexer.convertToHierarchy(data) match {
      case scala.util.Success(res) ⇒ res.flatMap(_._2)
      case scala.util.Failure(f) ⇒ throw f
    }
    res.map(_.asString).toSet
  }
}
