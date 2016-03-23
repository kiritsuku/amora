package indexer

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.net.URLEncoder

import scala.util.Try

import org.apache.jena.query.Dataset
import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QuerySolution
import org.apache.jena.query.ReadWrite
import org.apache.jena.query.ResultSetFactory
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.query.ResultSetRewindable
import org.apache.jena.rdf.model.Model
import org.apache.jena.tdb.TDBFactory

import indexer.hierarchy._

object Indexer {

  def queryResultAsString(modelName: String, query: String, model: Model): Try[String] = {
    withQueryService(modelName, query)(model) map { r ⇒
      val s = new ByteArrayOutputStream

      ResultSetFormatter.out(s, r)
      new String(s.toByteArray(), "UTF-8")
    }
  }

  def queryResult[A](modelName: String, query: String, model: Model)(f: (String, QuerySolution) ⇒ A): Try[Seq[A]] = {
    import scala.collection.JavaConverters._
    withQueryService(modelName, query)(model) map { r ⇒
      val vars = r.getResultVars.asScala.toSeq

      for { q ← r.asScala.toSeq; v ← vars } yield f(v, q)
    }
  }

  private def attachments(h: Hierarchy): String =
    h.attachments.map(_.asString).mkString("\"c:attachment\": [\"", "\", \"", "\"],")

  private def position(pos: Position) = pos match {
    case RangePosition(start, end) ⇒
      s""""c:start": $start, "c:end": $end,"""
    case _ ⇒
      ""
  }

  private def mkModel(filename: String)(h: Hierarchy): String = h match {
    case decl @ Decl(name, Root) ⇒
      s"""
        {
          "@id": "c:_root_/$name",
          "@type": "s:Text",
          "s:name": "$name",
          ${attachments(decl)}
          "c:tpe": "declaration",
          ${position(decl.position)}
          "c:file": "$filename",
          "c:parent": "c:_root_"
        }
      """

    case decl @ Decl(name, parent) ⇒
      val path = s"_root_/${parent.asString.replace('.', '/')}"
      val classEntry = s"""
        {
          "@id": "c:$path/$name",
          "@type": "s:Text",
          "s:name": "$name",
          ${attachments(decl)}
          "c:tpe": "declaration",
          ${position(decl.position)}
          "c:file": "$filename",
          "c:parent": "c:$path"
        }
      """
      val declEntry = mkModel(filename)(parent)
      Seq(classEntry, declEntry).mkString(",\n")

    case TermRef(name, outer) ⇒
      "[]"
    case ref @ TypeRef(usage, decl) ⇒
      val path = s"_root_/${decl.asString.replace('.', '/')}"
      val f = URLEncoder.encode(filename, "UTF-8")
      val h = ref.hashCode
      val u = s"_root_/${usage.asString.replace('.', '/')}"
      s"""
        {
          "@id": "c:$path/$f/$h",
          "@type": "s:Text",
          "c:tpe": "typeref",
          "c:file": "$filename",
          "c:reference": "c:$path",
          "c:usage": "c:$u"
        }
      """
    case ThisRef(cls) ⇒
      "[]"
    case Root ⇒
      "[]"
  }

  def add(modelName: String, filename: String, data: Seq[Hierarchy])(model: Model): Try[Unit] = Try {
    val str = s"""
      {
        "@context": {
          "c": "$modelName",
          "s": "http://schema.org/",
          "c:declaration": {
            "@id": "c:declaration",
            "@type": "@id"
          },
          "c:parent": {
            "@id": "c:parent",
            "@type": "@id"
          },
          "c:usage": {
            "@id": "c:usage",
            "@type": "@id"
          },
          "c:attachment": {
            "@id": "c:attachment"
          },
          "c:reference": {
            "@id": "c:reference",
            "@type": "@id"
          },
          "c:start": {
            "@id": "c:start"
          },
          "c:end": {
            "@id": "c:end"
          }
        },
        "@graph": [
          ${data map mkModel(filename) mkString ",\n"}
        ]
      }
    """
    val in = new ByteArrayInputStream(str.getBytes)
    model.read(in, /* base = */ null, "JSON-LD")
  }

  def withQueryService(modelName: String, query: String)(model: Model): Try[ResultSetRewindable] = Try {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), model)
    ResultSetFactory.makeRewindable(qexec.execSelect())
  }

  def withSparqlService(endpoint: String, query: String): Try[ResultSetRewindable] = Try {
    val qe = QueryExecutionFactory.sparqlService(endpoint, query)
    ResultSetFactory.makeRewindable(qe.execSelect())
  }

  def withInMemoryDataset[A](f: Dataset ⇒ A): Try[A] = {
    val dataset = TDBFactory.createDataset()
    internalWithDataset(dataset)(f)
  }

  def withDataset[A](location: String)(f: Dataset ⇒ A): Try[A] = {
    val dataset = TDBFactory.createDataset(location)
    internalWithDataset(dataset)(f)
  }

  def withModel[A](dataset: Dataset, name: String)(f: Model ⇒ A): Try[A] = {
    val model = dataset.getNamedModel(name)
    Try(model.begin())
      .map { _ ⇒ f(model) }
      .map { res ⇒ model.commit(); res }
      .map { res ⇒ model.close(); res }
  }

  private def internalWithDataset[A](dataset: Dataset)(f: Dataset ⇒ A): Try[A] = {
    Try(dataset.begin(ReadWrite.WRITE))
      .map { _ ⇒ f(dataset) }
      .map { res ⇒ dataset.commit(); res }
      .map { res ⇒ dataset.end(); res }
      .map { res ⇒ dataset.close(); res }
      .recover { case f ⇒ dataset.abort(); throw f }
  }
}
