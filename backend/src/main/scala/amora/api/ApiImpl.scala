package amora.api

import java.io.ByteArrayInputStream

import org.apache.jena.rdf.model.ModelFactory

class ApiImpl {

  def ttlModel(strings: Iterator[String], expressions: Iterator[Any]): SparqlModel = {
    val res = op(strings, expressions) {
      case str: String ⇒ escapeString(str)
      case obj ⇒ escapeString(obj.toString)
    }
    val m = ModelFactory.createDefaultModel()
    val in = new ByteArrayInputStream(res.getBytes)
    m.read(in, null, "TURTLE")
    new SparqlModel(m)
  }

  def sparqlQuery(strings: Iterator[String], expressions: Iterator[Any]): SparqlQuery = {
    val res = op(strings, expressions) {
      case str: String ⇒ escapeString(str)
      case obj ⇒ escapeString(obj.toString)
    }
    new SparqlQuery(res)
  }

  private def escapeString(str: String) =
    str.replace("\"", "\\\"").replace("\n", "\\n")

  private def op(strings: Iterator[String], expressions: Iterator[Any])(f: Any ⇒ String): String = {
    val sb = new StringBuilder(strings.next())
    while (strings.hasNext) {
      val e = f(expressions.next())
      sb append e
      sb append strings.next()
    }
    sb.toString()
  }
}
