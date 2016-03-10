package indexer

import java.io.ByteArrayInputStream

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

  val modelName = "testmodel"
  withDataset(s"$storageLocation/dataset") { dataset ⇒
    withModel(dataset, modelName)(add)
    withModel(dataset, modelName)(show)
  }

  def show(model: Model) = {
    val q2 = """SELECT * { ?s ?p ?o }"""
    val qexec = QueryExecutionFactory.create(QueryFactory.create(q2), model)
    val r = ResultSetFactory.makeRewindable(qexec.execSelect())

    ResultSetFormatter.out(System.out, r)
  }

  def add(model: Model) = {
    val data = s"""
      @prefix :<$modelName/> .
      @prefix foaf:<http://xmlns.com/foaf/0.1/> .

      :helloWorld
        a foaf:String;
        foaf:name "helloWorld"
        .
    """
    val in = new ByteArrayInputStream(data.getBytes)
    model.read(in, /* base = */ null, "TURTLE")
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
        model.abort()
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
