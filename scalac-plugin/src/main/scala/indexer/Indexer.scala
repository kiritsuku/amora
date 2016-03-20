package indexer

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.net.URL

import scala.util.Failure
import scala.util.Success
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
import org.apache.log4j.ConsoleAppender
import org.apache.log4j.Level
import org.apache.log4j.LogManager
import org.apache.log4j.PatternLayout

import indexer.hierarchy._

object Indexer extends App with LoggerConfig {

  /**
   * The location where we want to store our data. Since we can run the program
   * from different locations we need a stable location which never changes.
   * This method returns such a path.
   */
  def storageLocation = {
    val p = getClass.getProtectionDomain.getCodeSource.getLocation.getPath
    val projectName = "scalac-plugin/"
    val i = p.indexOf(projectName)
    p.substring(0, i+projectName.length)
  }

  private def addData(filename: String, data: Seq[Hierarchy]) = {
    val modelName = "http://test.model/"

    withDataset(s"$storageLocation/dataset") { dataset ⇒
      withModel(dataset, modelName)(add(modelName, filename, data))
      withModel(dataset, modelName) { model ⇒
        val q = findClass(modelName)
        println(queryResultAsString(modelName, q, model))
      }
    }
  }

  private def findClass(modelName: String) = s"""
    PREFIX c:<$modelName>
    PREFIX s:<http://schema.org/>
    SELECT ?elem ?p ?o WHERE {
      ?elem s:name ?name .
      FILTER (str(?name) = "SomeClass") .
      ?elem c:tpe "class" .
      ?elem c:declaration ?decl .
      FILTER regex(str(?decl), "_root_/.*/c") .
      ?elem ?p ?o .
    }
  """

  private def selectAll(modelName: String) = s"""
    SELECT * WHERE {
      ?s ?p ?o .
    }
  """

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

  private def httpRequest() = {
    val endpoint = new URL("http://dbpedia.org/sparql")
    val query = """
      PREFIX dbo: <http://dbpedia.org/ontology/>
      PREFIX dbp: <http://dbpedia.org/property/>
      SELECT DISTINCT ?language ?p ?o WHERE {
       ?language a dbo:ProgrammingLanguage .
       ?language dbp:name ?name .
       FILTER(STR(?name) = "Scala") .
       ?language ?p ?o .
      } LIMIT 100
    """

    withSparqlService(endpoint.toString(), query) map { r ⇒
      ResultSetFormatter.out(System.out, r)
    }
  }

  private def mkModel(filename: String)(h: Hierarchy): String = h match {
    case Package(pkgs) ⇒
      def pkgEntry(pkgs: Seq[String]) = s"""
        {
          "@id": "c:${pkgs.mkString("/")}",
          "@type": "s:Text",
          "s:name": "${pkgs.last}",
          "c:tpe": "package",
          "c:file": "$filename",
          "c:declaration": "c:${pkgs.init.mkString("/")}"
        }
      """
      val entries = ("_root_" +: pkgs).inits.toList.init map pkgEntry
      entries.mkString(",\n")

    case Class(decl, name) ⇒
      val path = s"_root_/${decl.toString.replace('.', '/')}"
      val classEntry = s"""
        {
          "@id": "c:$path/$name",
          "@type": "s:Text",
          "s:name": "$name",
          "c:tpe": "class",
          "c:file": "$filename",
          "c:declaration": "c:$path"
        }
      """
      val declEntry = mkModel(filename)(decl)
      Seq(classEntry, declEntry).mkString(",\n")

    case Member(parent, name) ⇒
      val path = s"_root_/${parent.toString.replace('.', '/')}"
      val memberEntry = s"""
        {
          "@id": "c:$path/$name",
          "@type": "s:Text",
          "s:name": "$name",
          "c:tpe": "member",
          "c:file": "$filename",
          "c:parent": "c:$path"
        }
      """
      val parentEntry = mkModel(filename)(parent)
      Seq(memberEntry, parentEntry).mkString(",\n")
    case TermRef(name, outer) ⇒
      "[]"
    case TypeRef(_, decl) ⇒
      "[]"
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

trait LoggerConfig {
  val layout = new PatternLayout("%d %5p [%t] - %c - %m%n")
  val consoleAppender = new ConsoleAppender(layout, ConsoleAppender.SYSTEM_OUT)
  val rootLogger = LogManager.getRootLogger
  rootLogger.setLevel(Level.INFO)
  rootLogger.addAppender(consoleAppender)
}
