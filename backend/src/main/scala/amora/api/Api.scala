package amora.api

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QuerySolution
import org.apache.jena.query.ResultSetFactory
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.query.ResultSetRewindable
import org.apache.jena.rdf.model.Model

final class SparqlQuery(val query: String) {
  def runOnModel(model: SparqlModel): SparqlResultSet = {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), model.model)
    new SparqlResultSet(ResultSetFactory.makeRewindable(qexec.execSelect()))
  }
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
}

final class SparqlModel(val model: Model)

final class ResultSetRow(val row: QuerySolution) {
  def string(varName: String): String =
    row.get(varName).asLiteral.getString

  def int(varName: String): Int =
    row.get(varName).asLiteral.getInt

  def long(varName: String): Long =
    row.get(varName).asLiteral.getLong

  def boolean(varName: String): Boolean =
    row.get(varName).asLiteral.getBoolean

  def double(varName: String): Double =
    row.get(varName).asLiteral.getDouble

  def float(varName: String): Float =
    row.get(varName).asLiteral.getFloat

  def char(varName: String): Char =
    row.get(varName).asLiteral.getChar

  def byte(varName: String): Byte =
    row.get(varName).asLiteral.getByte

  def uri(varName: String): String =
    row.get(varName).toString
}
