package amora.api

import org.apache.jena.rdf.model.ModelFactory

class ApiImpl {

  def turtleModelFromString(model: String): SparqlModel =
    new SparqlModel(ModelFactory.createDefaultModel()).writeAs(Turtle, model)

  def turtleModel(strings: Iterator[String], expressions: Iterator[Any]): SparqlModel = {
    val res = op(strings, expressions) {
      case str: String ⇒ escapeString(str)
      case obj ⇒ obj.toString
    }
    turtleModelFromString(res)
  }

  def sparqlQuery(strings: Iterator[String], expressions: Iterator[Any]): SparqlQuery = {
    val res = op(strings, expressions) {
      case str: String ⇒ escapeString(str)
      case obj ⇒ obj.toString
    }
    new SparqlQuery(res)
  }

  def sparql(strings: Iterator[String], expressions: Iterator[Any]): String = {
    op(strings, expressions) {
      case str: String ⇒ escapeString(str)
      case obj ⇒ obj.toString
    }
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
