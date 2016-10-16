package amora.backend.indexer

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

import org.apache.jena.datatypes.BaseDatatype
import org.apache.jena.query.Dataset
import org.apache.jena.query.ParameterizedSparqlString
import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QuerySolution
import org.apache.jena.query.ReadWrite
import org.apache.jena.query.ResultSetFactory
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.query.ResultSetRewindable
import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.tdb.TDBFactory
import org.apache.jena.update.UpdateAction

import amora.backend.Log4jLogging
import spray.json._

class Indexer(modelName: String) extends Log4jLogging {

  /**
   * On startup of the indexer, we want to index some predefined data like
   * schema definitions.
   */
  def startupIndexer(dataset: Dataset): Unit = {
    import java.io.File

    def indexServices(model: Model) = {
      def findRoot(dir: File): File =
        if (dir.listFiles().exists(_.getName == ".git"))
          dir
        else
          findRoot(dir.getParentFile)
      val root = findRoot(new File(getClass.getClassLoader.getResource(".").getPath))

      def service(dir: String) = new File(root.getAbsolutePath + dir)
      val serviceDirectories = Seq(
        service("/services/scala-compiler"),
        service("/services/scalac"),
        service("/services/dotc"),
        service("/schema")
      )
      val serviceFiles = serviceDirectories flatMap { serviceDirectory ⇒
        serviceDirectory.listFiles().filter(_.getName.endsWith(".service.ttl"))
      }

      addTurtle(model, s"""
        @prefix service:<http://amora.center/kb/Schema/Service/0.1/> .
        @prefix registry:<http://amora.center/kb/Service/0.1/> .
        ${
          serviceFiles.map { s ⇒
            val name = s.getName.dropRight(".service.ttl".length)
            s"""
        registry:$name
          a service: ;
          service:name "$name" ;
          service:path "${s.getCanonicalPath}" ;
          service:directory "${s.getParentFile.getAbsolutePath}" ;
          service:fileName "${s.getName}" ;
        ."""
          }.mkString
        }
      """)
    }

    def indexSchemas(model: Model) = {
      val cl = getClass.getClassLoader
      val resourceDir = new File(cl.getResource(".").getPath)
      val indexableFiles = resourceDir.listFiles().filter(_.getName.endsWith(".schema.ttl"))
      indexableFiles foreach { file ⇒
        val src = io.Source.fromFile(file, "UTF-8")
        val content = src.mkString
        val schemaName = file.getName.dropRight(".schema.ttl".length)
        src.close()
        val alreadyIndexed = runAskQuery(model, s"""
          ASK {
            <http://amora.center/kb/amora/Schema/0.1/$schemaName/0.1/> <http://amora.center/kb/amora/Schema/0.1/schemaVersion> ?o
          }
        """)
        if (!alreadyIndexed) {
          addTurtle(model, content)
          log.info(s"Schema file `$file` successfully indexed.")
        }
      }
    }

    def indexJsonLdFormat(model: Model) = {
      val cl = getClass.getClassLoader
      val resourceDir = new File(cl.getResource(".").getPath)
      val indexableFiles = resourceDir.listFiles().filter(_.getName.endsWith(".schema.jsonld"))
      val gen = new SchemaGenerator

      def indexFile(file: File) = {
        val src = io.Source.fromFile(file, "UTF-8")
        val rawJson = src.mkString
        val schemaName = file.getName.dropRight(".schema.jsonld".length)
        src.close()

        val alreadyIndexed = doesIdExist(model, gen.mkAmoraSchemaId(schemaName)+"/")
        if (!alreadyIndexed) {
          val json = gen.resolveVariables(schemaName, rawJson)
          val contentVar = "content"
          withUpdateService(model, gen.mkInsertFormatQuery(schemaName, contentVar)) { pss ⇒
            pss.setLiteral(contentVar, gen.mkJsonLdContext(schemaName, json).prettyPrint, new BaseDatatype("http://schema.org/Text"))
          }
          log.info(s"Schema file `$file` successfully indexed.")
        }
      }

      indexableFiles foreach indexFile
    }

    try withModel(dataset) { model ⇒
      indexJsonLdFormat(model)
      indexSchemas(model)
      indexServices(model)

      log.info("Indexer successfully started.")
    } catch {
      case t: Throwable ⇒
        throw new RuntimeException("An error happened during initialization of the indexer.", t)
    }
  }

  def queryResultAsString(query: String, model: Model): String = {
    val r = withQueryService(model, query)
    val s = new ByteArrayOutputStream

    ResultSetFormatter.out(s, r)
    new String(s.toByteArray(), "UTF-8")
  }

  def flattenedQueryResult[A](query: String, model: Model)(f: (String, QuerySolution) ⇒ A): Seq[A] = {
    import scala.collection.JavaConverters._
    val r = withQueryService(model, query)
    val vars = r.getResultVars.asScala.toSeq

    for { q ← r.asScala.toSeq; v ← vars } yield f(v, q)
  }

  def queryResult[A](query: String, model: Model)(f: (String, QuerySolution) ⇒ A): Seq[Seq[A]] = {
    import scala.collection.JavaConverters._
    val r = withQueryService(model, query)
    val vars = r.getResultVars.asScala.toSeq

    for (q ← r.asScala.toSeq) yield
      for (v ← vars) yield
        f(v, q)
  }

  def addJsonLd(model: Model, data: JsValue): Unit = {
    val str = data.prettyPrint
    val in = new ByteArrayInputStream(str.getBytes)
    model.read(in, /* base = */ null, "JSON-LD")
  }

  def addTurtle(model: Model, str: String): Unit = {
    val in = new ByteArrayInputStream(str.getBytes)
    model.read(in, /* base = */ null, "TURTLE")
  }

  def withUpdateService(model: Model, query: String)(f: ParameterizedSparqlString ⇒ Unit): Unit = {
    val pss = new ParameterizedSparqlString
    pss.setCommandText(query)
    f(pss)
    val update = pss.asUpdate()
    UpdateAction.execute(update, model)
  }

  def doesIdExist(model: Model, id: String): Boolean =
    runAskQuery(model, s"ASK { <$id> ?p ?o }")

  def runAskQuery(model: Model, query: String): Boolean = {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), model)
    qexec.execAsk()
  }

  def withQueryService(model: Model, query: String): ResultSetRewindable = {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), model)
    ResultSetFactory.makeRewindable(qexec.execSelect())
  }

  def withSparqlService(endpoint: String, query: String): ResultSetRewindable = {
    val qe = QueryExecutionFactory.sparqlService(endpoint, query)
    ResultSetFactory.makeRewindable(qe.execSelect())
  }

  def mkInMemoryDataset: RawDataset =
    RawDataset(TDBFactory.createDataset())

  def mkDataset(location: String): RawDataset =
    RawDataset(TDBFactory.createDataset(location))

  def writeDataset[A](dataset: RawDataset)(f: Dataset ⇒ A): A = {
    internalWithDataset(dataset.dataset, ReadWrite.WRITE)(f)
  }

  def readDataset[A](dataset: RawDataset)(f: Dataset ⇒ A): A = {
    internalWithDataset(dataset.dataset, ReadWrite.READ)(f)
  }

  def withModel[A](dataset: Dataset)(f: Model ⇒ A): A = {
    val model = ModelFactory.createRDFSModel(dataset.getNamedModel(modelName))
    model.begin()
    try {
      val res = f(model)
      model.commit()
      res
    } finally {
      model.close()
    }
  }

  private def internalWithDataset[A](dataset: Dataset, op: ReadWrite)(f: Dataset ⇒ A): A = {
    dataset.begin(op)
    try {
      val res = f(dataset)
      dataset.commit()
      res
    } finally {
      dataset.end()
    }
  }
}

final case class RawDataset(dataset: Dataset) {
  def close(): Unit =
    dataset.close()
}
