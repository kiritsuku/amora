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

  def addData(filename: String, data: Seq[Hierarchy]) = {
    val modelName = "http://test.model/"

    withDataset(s"$storageLocation/dataset") { dataset ⇒
      withModel(dataset, modelName)(add(modelName, filename, data))
      withModel(dataset, modelName)(testShow(modelName))
    }
  }

  private def findAllMethodsOfMathedClasses(modelName: String)(regex: String) = s"""
    PREFIX c:<$modelName>
    PREFIX s:<http://schema.org/>
    SELECT * WHERE {
      ?class c:tpe "class" .
      ?class s:name ?className .
      FILTER regex(str(?className), "$regex") .
      ?member c:parent ?class .
    }
  """

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

  private def testShow(modelName: String)(model: Model) = {
    val q = findAllMethodsOfMathedClasses(modelName)(".*Class.*")
    println(queryResultAsString(modelName, q, model))
  }

  def queryResultAsString(modelName: String, query: String, model: Model): String = {
    val r = withQueryService(modelName, query)(model)
    println(r.getResultVars)
    println(r.next().get("class"))
    val s = new ByteArrayOutputStream

    ResultSetFormatter.out(s, r)
    new String(s.toByteArray())
  }

  def queryResult[A](modelName: String, query: String, model: Model)(f: (String, QuerySolution) ⇒ A): Seq[A] = {
    import scala.collection.JavaConverters._
    val r = withQueryService(modelName, query)(model)
    val vars = r.getResultVars.asScala.toSeq

    for { q ← r.asScala.toSeq; v ← vars } yield f(v, q)
  }

  def httpRequest() = {
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

    val r = withSparqlService(endpoint.toString(), query)
    ResultSetFormatter.out(System.out, r)
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

  def add(modelName: String, filename: String, data: Seq[Hierarchy])(model: Model) = {
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

  def withQueryService(modelName: String, query: String)(model: Model): ResultSetRewindable = {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), model)
    ResultSetFactory.makeRewindable(qexec.execSelect())
  }

  def withSparqlService(endpoint: String, query: String): ResultSetRewindable = {
    val qe = QueryExecutionFactory.sparqlService(endpoint, query)
    ResultSetFactory.makeRewindable(qe.execSelect())
  }

  def withInMemoryDataset(f: Dataset ⇒ Unit): Unit = {
    val dataset = TDBFactory.createDataset()
    dataset.begin(ReadWrite.WRITE)
    Try(f(dataset)) match {
      case Success(v) ⇒
        dataset.commit()
      case Failure(f) ⇒
        f.printStackTrace()
        dataset.abort()
    }
    dataset.end()
    dataset.close()
  }

  def withDataset(location: String)(f: Dataset ⇒ Unit): Unit = {
    val dataset = TDBFactory.createDataset(location)
    dataset.begin(ReadWrite.WRITE)
    Try(f(dataset)) match {
      case Success(v) ⇒
        dataset.commit()
      case Failure(f) ⇒
        f.printStackTrace()
        dataset.abort()
    }
    dataset.end()
    dataset.close()
  }

  def withModel(dataset: Dataset, name: String)(f: Model ⇒ Unit): Unit = {
    val model = dataset.getNamedModel(name)
    model.begin()
    Try(f(model)) match {
      case Success(v) ⇒
        model.commit()
      case Failure(f) ⇒
        f.printStackTrace()
        // doesn't seem to be supported
        // model.abort()
    }
    model.close()
  }
}

trait LoggerConfig {
  val layout = new PatternLayout("%d %5p [%t] - %c - %m%n")
  val consoleAppender = new ConsoleAppender(layout, ConsoleAppender.SYSTEM_OUT)
  val rootLogger = LogManager.getRootLogger
  rootLogger.setLevel(Level.INFO)
  rootLogger.addAppender(consoleAppender)
}
