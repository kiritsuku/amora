package amora.backend.indexer

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

import scala.util.control.NonFatal

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

import amora.api.SparqlResultSet
import amora.backend.Log4jLogging
import amora.nlp._
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
        service("/schema"),
        service("/converter/protocol"),
        service("/converter/scalac")
      )
      val serviceFiles = serviceDirectories flatMap { serviceDirectory ⇒
        serviceDirectory.listFiles().filter(_.getName.endsWith(".service.ttl"))
      }

      addTurtle(model, s"""
        @prefix service:<http://amora.center/kb/Schema/Service/> .
        @prefix registry:<http://amora.center/kb/Service/> .
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
            <http://amora.center/kb/amora/Schema/$schemaName/> <http://amora.center/kb/amora/Schema/schemaVersion> ?o
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

  def askNlq(model: Model, query: String): String = {
    val s = NlParser.parseQuery(query)

    var prefixe = Map[String, String]()
    var data = Map[String, Map[String, Set[String]]]()
    var selects = Set[String]()
    var visualization = "list"

    def addPrefix(name: String, url: String) = {
      if (!prefixe.contains(name))
        prefixe += name → url
    }

    def addData(variable: String, k: String, v: String) = {
      data.get(variable) match {
        case Some(map) ⇒
          val set = map.getOrElse(k, Set())
          data += variable → (map + k → (set + v))
        case None ⇒
          data += variable → Map(k → Set(v))
      }
    }

    def addSelect(name: String) = {
      selects += name
    }

    def lookupNounAsProperty(noun: Noun, id: String, classSchema: String) = {
      import amora.api._
      val q = sparqlQuery"""
        prefix Semantics:<http://amora.center/kb/amora/Schema/Semantics/>
        prefix Schema:<http://amora.center/kb/amora/Schema/>
        select ?schema ?name where {
          [Semantics:word "${noun.word}"] Semantics:association ?schema .
          $classSchema Schema:schemaId ?schema .
          ?schema Schema:schemaName ?name .
        }
      """
      log.info(s"Ask for semantic information about `${noun.word}` (noun,property):\n${q.query}")
      val (schema, name) = q.runOnModel(new SparqlModel(model)).map { r ⇒
        (r.uri("schema"), r.string("name"))
      }.head
      addData(id, schema, s"?$name")
      addSelect(name)
    }

    def lookupNounAsClass(noun: Noun) = {
      import amora.api._
      val q = sparqlQuery"""
        prefix Semantics:<http://amora.center/kb/amora/Schema/Semantics/>
        prefix Schema:<http://amora.center/kb/amora/Schema/>
        select ?schema ?name where {
          [Semantics:word "${noun.word}"] Semantics:association ?schema .
          ?schema Schema:schemaName ?name .
        }
      """
      log.info(s"Ask for semantic information about `${noun.word}` (noun,class):\n${q.query}")
      val (schema, name) = q.runOnModel(new SparqlModel(model)).map { r ⇒
        (r.uri("schema"), r.string("name"))
      }.head
      val selectId = s"x${data.size}"
      addPrefix(name, schema)
      addData(selectId, "a", s"$name:")
      (selectId, schema)
    }

    def lookupVisualization(noun: Noun) = {
      import amora.api._
      val q = sparqlQuery"""
        prefix Visualization:<http://amora.center/kb/amora/Schema/Visualization/>
        select ?v where {
          ?v Visualization:word "${noun.word}" .
        }
      """
      log.info(s"Ask for visualization information about `${noun.word}`:\n${q.query}")
      // right now we only need to check if the visualization exists
      val _ = q.runOnModel(new SparqlModel(model)).map { r ⇒
        r.uri("v")
      }.head
      visualization = noun.word
    }

    def lookupPreposition(property: Noun, pp: PrepositionPhrase) = {
      val (id, schema) = lookupNounAsClass(pp.noun)
      lookupNounAsProperty(property, id, schema)
      pp.remaining match {
        case Some(n: Noun) ⇒
          lookupGrammar(id, schema, n.original)
        case Some(outer: PrepositionPhrase) ⇒
          outer.preposition match {
            case Preposition("as") ⇒
              lookupVisualization(outer.noun)
            case _ ⇒
              lookupInnerPreposition(id, schema, outer)
          }
        case None ⇒
        case node ⇒
          throw new IllegalStateException(s"Unknown tree node $node.")
      }
    }
    def lookupInnerPreposition(idInner: String, schemaInner: String, pp: PrepositionPhrase): Unit = {
      val (id, schema) = lookupNounAsClass(pp.noun)
      addData(idInner, s"${schemaInner.init}owner>", s"?$id")
      pp.remaining match {
        case Some(n: Noun) ⇒
          lookupGrammar(id, schema, n.original)
        case Some(outer: PrepositionPhrase) ⇒
          lookupInnerPreposition(id, schema, outer)
        case None ⇒
        case node ⇒
          throw new IllegalStateException(s"Unknown tree node $node.")
      }
    }

    def lookupVerb(verb: Verb) = {
      if (verb.word == "list" || verb.word == "show")
        ()
      else
        ???
    }

    def lookupGrammar(id: String, schema: String, word: String) = {
      schema match {
        case "<http://amora.center/kb/amora/Schema/Class/>" ⇒
          if (NlParser.isScalaIdent(word)) {
            addData(id, "<http://amora.center/kb/amora/Schema/Class/name>", "\"" + word + "\"")
          }
        case "<http://amora.center/kb/amora/Schema/Def/>" ⇒
          if (NlParser.isScalaIdent(word)) {
            addData(id, "<http://amora.center/kb/amora/Schema/Def/name>", "\"" + word + "\"")
          }
        case name ⇒
          throw new IllegalStateException(s"No grammar handling for class `$name` found.")
      }
    }

    def mkSparql = {
      val stringOrdering = new Ordering[String] {
        def compare(a: String, b: String) = String.CASE_INSENSITIVE_ORDER.compare(a, b)
      }

      val sb = new StringBuilder
      prefixe.toList.sortBy(_._1)(stringOrdering) foreach {
        case (name, url) ⇒
          sb append "prefix " append name append ":" append url append "\n"
      }
      sb append "select"
      selects foreach (select ⇒ sb append " ?" append select)
      sb append " where {\n"
      data.toList.sortBy(_._1)(stringOrdering) foreach {
        case (variable, kv) ⇒
          kv.toList.sortBy(_._1)(stringOrdering) foreach {
            case (k, set) ⇒
              set foreach { v ⇒
                sb append "  ?" append variable append " " append k append " " append v append " .\n"
              }
          }
      }
      sb append "}"
      sb.toString
    }

    s.remaining match {
      case Some(pp: PrepositionPhrase) ⇒
        lookupPreposition(s.noun, pp)
      case Some(n: Noun) ⇒
        val (id, schema) = lookupNounAsClass(s.noun)
        addSelect(id)
        lookupGrammar(id, schema, n.original)
      case None ⇒
        val (id, _) = lookupNounAsClass(s.noun)
        addSelect(id)
      case node ⇒
        throw new IllegalStateException(s"Unknown tree node $node.")
    }
    lookupVerb(s.verb)

    val sparqlQuery = mkSparql
    log.info(s"Natural language query `$query` as SPARQL query:\n$sparqlQuery")
    val rs = withQueryService(model, sparqlQuery)
    val srs = new SparqlResultSet(rs)

    def mkTurtle = {
      val sb = new StringBuilder
      sb append "@prefix VResponse:<http://amora.center/kb/amora/Schema/VisualizationResponse/> .\n"
      sb append "@prefix VGraph:<http://amora.center/kb/amora/Schema/VisualizationGraph/> .\n"
      sb append "<#this>\n"
      sb append "  a VResponse: ;\n"
      visualization match {
        case "list" ⇒
          sb append "  VResponse:graph"
          srs foreach { rs ⇒
            val v = rs.row.get(selects.head)
            val str = if (v.isLiteral()) v.asLiteral().getString else v.toString()
            sb append " [\n    VGraph:value \"" append str append "\" ;\n  ],"
          }
          sb append " [] ;\n"
      }
      sb append ".\n"
      val vresp = sb.toString()
      log.info(s"Visualization response for natural language query `$query`:\n$vresp")
      vresp
    }

    mkTurtle
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
    // no catch here because 'abort' on models is not supported
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
    } catch {
      case NonFatal(e) ⇒
        dataset.abort()
        throw e
    } finally {
      dataset.end()
    }
  }
}

final case class RawDataset(dataset: Dataset) {
  def close(): Unit =
    dataset.close()
}
