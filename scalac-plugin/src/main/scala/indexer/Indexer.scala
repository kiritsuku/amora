package indexer

import java.io.ByteArrayInputStream
import java.net.URL

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.apache.jena.query.Dataset
import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.ReadWrite
import org.apache.jena.query.ResultSetFactory
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.rdf.model.Model
import org.apache.jena.tdb.TDBFactory
import org.apache.log4j.ConsoleAppender
import org.apache.log4j.Level
import org.apache.log4j.LogManager
import org.apache.log4j.PatternLayout

import indexer.hierarchy.Hierarchy

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

  def addData(data: Seq[Hierarchy]) = {
    val modelName = "http://test.model/"

    withDataset(s"$storageLocation/dataset") { dataset ⇒
      withModel(dataset, modelName)(add(modelName, data))
      withModel(dataset, modelName)(show)
    }
  }

  def show(model: Model) = {
    val q = s"""
      select * { ?s ?p ?o }
    """
    val qexec = QueryExecutionFactory.create(QueryFactory.create(q), model)
    val r = ResultSetFactory.makeRewindable(qexec.execSelect())

    ResultSetFormatter.out(System.out, r)
  }

  def httpRequest() = {
    val endpoint = new URL("http://dbpedia.org/sparql")
    val query = """
      PREFIX ont: <http://dbpedia.org/ontology/>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      SELECT DISTINCT ?language ?p ?o WHERE {
       ?language a ont:ProgrammingLanguage .
       ?language ont:name ?name .
       ?name rdfs:label "Scala" .
       ?language ?p ?o .
      } LIMIT 100
    """
    val r = withSparqlService(endpoint.toString(), query)
    ResultSetFormatter.out(System.out, r)
  }

  def add(modelName: String, data: Seq[Hierarchy])(model: Model) = {
    val data = s"""{
        "@context": {
          "c": "$modelName",
          "s": "http://schema.org/"
        },
        "@id": "c:test",
        "@type": "s:Text",
        "s:name": "test"
    }"""
    val in = new ByteArrayInputStream(data.getBytes)
    model.read(in, /* base = */ null, "JSON-LD")
  }

  def withSparqlService(endpoint: String, query: String) = {
    val qe = QueryExecutionFactory.sparqlService(endpoint, query)
    ResultSetFactory.makeRewindable(qe.execSelect())
  }

  def withDataset(location: String)(f: Dataset ⇒ Unit) = {
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

  def withModel(dataset: Dataset, name: String)(f: Model ⇒ Unit) = {
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
