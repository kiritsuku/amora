package amora.api

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QuerySolution
import org.apache.jena.query.ResultSetFactory
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.query.ResultSetRewindable
import org.apache.jena.rdf.model.{ Literal ⇒ JLiteral }
import org.apache.jena.rdf.model.Model

final class SparqlQuery(val query: String) {
  def runOnModel(model: SparqlModel): SparqlResultSet = {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), model.model)
    new SparqlResultSet(ResultSetFactory.makeRewindable(qexec.execSelect()))
  }

  def askOnModel(model: SparqlModel): Boolean = {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), model.model)
    qexec.execAsk()
  }

  override def toString = query
}

final class SparqlResultSet(val resultSet: ResultSetRewindable) {
  def asStringTable: String = {
    val s = new ByteArrayOutputStream
    ResultSetFormatter.out(s, resultSet)
    val ret = new String(s.toByteArray(), StandardCharsets.UTF_8)
    resultSet.reset()
    ret
  }

  def map[A](f: ResultSetRow ⇒ A): Seq[A] = {
    import scala.collection.JavaConverters._
    val ret = resultSet.asScala.map(row ⇒ f(new ResultSetRow(row))).toList
    resultSet.reset()
    ret
  }

  def foreach(f: ResultSetRow ⇒ Unit): Unit = {
    import scala.collection.JavaConverters._
    resultSet.asScala.foreach(row ⇒ f(new ResultSetRow(row)))
    resultSet.reset()
  }
}

final class SparqlModel(val model: Model)

final class ResultSetRow(val row: QuerySolution) {
  def string(varName: String): String =
    get(varName).asLiteral.getString

  def int(varName: String): Int =
    get(varName).asLiteral.getInt

  def long(varName: String): Long =
    get(varName).asLiteral.getLong

  def boolean(varName: String): Boolean =
    get(varName).asLiteral.getBoolean

  def double(varName: String): Double =
    get(varName).asLiteral.getDouble

  def float(varName: String): Float =
    get(varName).asLiteral.getFloat

  def char(varName: String): Char =
    get(varName).asLiteral.getChar

  def byte(varName: String): Byte =
    get(varName).asLiteral.getByte

  def uri(varName: String): String = {
    val v = get(varName)
    if (v.isLiteral())
      throw new IllegalStateException(s"Value of variable name `$varName` is not an URI, it is of type: ${v.asLiteral().getDatatypeURI}.")
    if (v.isAnon())
      s"<_:$v>"
    else
      s"<$v>"
  }

  def literal(varName: String): Literal = {
    val v = get(varName)
    if (v.isLiteral())
      Literal(v.asLiteral())
    else
      throw new IllegalArgumentException(s"The variable `$varName` does not contain a literal.")
  }

  private def get(varName: String) = {
    val v = row.get(varName)
    if (v == null)
      throw new IllegalArgumentException(s"The variable `$varName` does not exist in the result set.")
    v
  }
}

final case class Literal(literal: JLiteral) {
  def string: String = literal.getString
  def int: Int = literal.getInt
  def long: Long = literal.getLong
  def boolean: Boolean = literal.getBoolean
  def double: Double = literal.getDouble
  def float: Float = literal.getFloat
  def char: Char = literal.getChar
  def byte: Byte = literal.getByte

  def stringOpt: Option[String] =
    if (literal.getDatatype.getURI() == "http://www.w3.org/2001/XMLSchema#string")
      Some(string)
    else
      None

}
