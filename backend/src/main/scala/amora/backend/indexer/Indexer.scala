package amora.backend.indexer

import java.io.ByteArrayOutputStream

import scala.util.control.NonFatal

import org.apache.jena.query.Dataset
import org.apache.jena.query.ParameterizedSparqlString
import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QuerySolution
import org.apache.jena.query.ReadWrite
import org.apache.jena.query.ResultSetFactory
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.query.ResultSetRewindable
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.tdb.TDBFactory
import org.apache.jena.update.UpdateAction

import amora.api._
import amora.backend.Log4jLogging
import amora.nlp._

class Indexer(modelName: String) extends Log4jLogging {

  /**
   * On startup of the indexer, we want to index some predefined data like
   * schema definitions.
   */
  def startupIndexer(dataset: Dataset): Unit = {
    import java.io.File

    def indexServices(model: SparqlModel) = {
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

    def indexSchemas(model: SparqlModel) = {
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
          try addTurtle(model, content)
          catch {
            case NonFatal(t) ⇒
              throw new RuntimeException(s"An error happened during indexing of file `$file`", t)
          }
          log.info(s"Schema file `$file` successfully indexed.")
        }
      }
    }

    try withModel(dataset) { model ⇒
      indexSchemas(model)
      indexServices(model)

      log.info("Indexer successfully started.")
    } catch {
      case NonFatal(t) ⇒
        throw new RuntimeException("An error happened during initialization of the indexer.", t)
    }
  }

  def queryResultAsString(query: String, model: SparqlModel): String = {
    val r = withQueryService(model, query)
    val s = new ByteArrayOutputStream

    ResultSetFormatter.out(s, r)
    new String(s.toByteArray(), "UTF-8")
  }

  def flattenedQueryResult[A](query: String, model: SparqlModel)(f: (String, QuerySolution) ⇒ A): Seq[A] = {
    import scala.collection.JavaConverters._
    val r = withQueryService(model, query)
    val vars = r.getResultVars.asScala.toSeq

    for { q ← r.asScala.toSeq; v ← vars } yield f(v, q)
  }

  def queryResult[A](query: String, model: SparqlModel)(f: (String, QuerySolution) ⇒ A): Seq[Seq[A]] = {
    import scala.collection.JavaConverters._
    val r = withQueryService(model, query)
    val vars = r.getResultVars.asScala.toSeq

    for (q ← r.asScala.toSeq) yield
      for (v ← vars) yield
        f(v, q)
  }

  private def mkHash(model: SparqlModel, diff: SparqlModel): String = {
    val diffStr = diff.formatAs(NTriple).split("\n").sorted.mkString("\n")
    val blockchain = for (head ← headCommit(model)) yield head + "\n" + diffStr
    val strToHash = blockchain.getOrElse(diffStr)
    Utils.mkSha256(strToHash)
  }

  private def commitModelName(commit: String) =
    s"http://amora.center/kb/amora/Commit/$commit"

  def writeAs(dataset: Dataset, model: SparqlModel, format: RdfFormat, data: String): Unit = {
    val diff = turtleModel(data).difference(model)
    val prevHash = headCommitUri(model)
    val hash = mkHash(model, diff)
    val storedModel = dataset.getNamedModel(commitModelName(hash))
    storedModel.add(diff.model)

    model.writeAs(Turtle, data)
    prevHash match {
      case Some(prevHash) ⇒
        model.writeAs(Turtle, s"""
          @prefix Commit:<http://amora.center/kb/amora/Schema/Commit/>
          @prefix CommitData:<http://amora.center/kb/amora/Commit/>
          CommitData:$hash
            a Commit: ;
            Commit:hash "$hash" ;
            Commit:previous <$prevHash> ;
          .
        """)
      case None ⇒
        model.writeAs(Turtle, s"""
          @prefix Commit:<http://amora.center/kb/amora/Schema/Commit/>
          @prefix CommitData:<http://amora.center/kb/amora/Commit/>
          CommitData:$hash
            a Commit: ;
            Commit:hash "$hash" ;
          .
        """)
    }
  }

  def addTurtle(model: SparqlModel, str: String): Unit = {
    model.writeAs(Turtle, str)
  }

  def askNlq(smodel: SparqlModel, query: String): String = {
    trait SelectType
    case object Id extends SelectType
    case object Value extends SelectType
    case object Rel extends SelectType

    var prefixe = Map[String, String]()
    var data = Map[String, Map[String, Set[String]]]()
    var selects = Map[SelectType, String]()
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

    def addSelect(tpe: SelectType, name: String) = {
      selects += tpe → name
    }

    def lookupNounAsProperty(noun: Noun, id: String, classSchema: String) = {
      val q = sparqlQuery"""
        prefix Semantics:<http://amora.center/kb/amora/Schema/Semantics/>
        prefix Schema:<http://amora.center/kb/amora/Schema/>
        select ?schema ?name where {
          [Semantics:word "${noun.word}"] Semantics:association ?schema .
          $classSchema Schema:schemaId ?schema .
          ?schema Schema:schemaName ?name .
        }
      """
      log.info(s"Ask for semantic information about `${noun.word}` (noun,property):\n$q")
      val (schema, name) = q.runOnModel(smodel).map { r ⇒
        (r.uri("schema"), r.string("name"))
      }.head
      addData(id, schema, s"?$name")
      addSelect(Value, name)
    }

    def lookupNounAsClass(noun: Noun) = {
      val q = sparqlQuery"""
        prefix Semantics:<http://amora.center/kb/amora/Schema/Semantics/>
        prefix Schema:<http://amora.center/kb/amora/Schema/>
        select ?schema ?name where {
          [Semantics:word "${noun.word}"] Semantics:association ?schema .
          ?schema Schema:schemaName ?name .
        }
      """
      log.info(s"Ask for semantic information about `${noun.word}` (noun,class):\n$q")
      val (schema, name) = q.runOnModel(smodel).map { r ⇒
        (r.uri("schema"), r.string("name"))
      }.head
      val selectId = s"x${data.size}"
      addPrefix(name, schema)
      addData(selectId, "a", s"$name:")
      addSelect(Id, selectId)
      (selectId, schema)
    }

    def lookupVisualization(noun: Noun) = {
      val q = sparqlQuery"""
        prefix Visualization:<http://amora.center/kb/amora/Schema/Visualization/>
        select ?v where {
          ?v Visualization:word "${noun.word}" .
        }
      """
      log.info(s"Ask for visualization information about `${noun.word}`:\n$q")
      // right now we only need to check if the visualization exists
      val _ = q.runOnModel(smodel).map { r ⇒
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

    def findRelationshipInformation() = {
      val ids = sparqlQuery"""
        prefix Decl:<http://amora.center/kb/amora/Schema/Decl/>
        prefix Schema:<http://amora.center/kb/amora/Schema/>
        select ?id where {
          Decl: Schema:schemaId ?id .
          ?id Schema:schemaType Decl: .
        }
      """.runOnModel(smodel).map { rs ⇒
        rs.uri("id")
      }
      if (ids.nonEmpty)
        log.info("Found relationship predicates: " + ids.mkString(", "))
      ids foreach { id ⇒
        val selectId = s"x${data.size}"
        addSelect(Rel, selectId)
        addData(selects(Id), id, s"?$selectId")
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
      selects.values foreach (select ⇒ sb append " ?" append select)
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
      new SparqlQuery(sb.toString)
    }

    def handleNlQuery() = {
      val s = NlParser.parseQuery(query)
      s.remaining match {
        case Some(pp: PrepositionPhrase) ⇒
          lookupPreposition(s.noun, pp)
        case Some(n: Noun) ⇒
          val (id, schema) = lookupNounAsClass(s.noun)
          lookupGrammar(id, schema, n.original)
        case None ⇒
          lookupNounAsClass(s.noun)
        case node ⇒
          throw new IllegalStateException(s"Unknown tree node $node.")
      }
      lookupVerb(s.verb)
    }

    def mkTurtle = {
      val sparqlQuery = mkSparql
      log.info(s"Natural language query `$query` as SPARQL query:\n$sparqlQuery")
      val srs = sparqlQuery.runOnModel(smodel)

      val sb = new StringBuilder
      sb append "@prefix VResponse:<http://amora.center/kb/amora/Schema/VisualizationResponse/> .\n"
      sb append "@prefix VGraph:<http://amora.center/kb/amora/Schema/VisualizationGraph/> .\n"
      sb append "<#this>\n"
      sb append "  a VResponse: ;\n"
      visualization match {
        case "list" ⇒
          sb append "  VResponse:graph"
          for (rs ← srs) {
            val v = rs.row.get(selects.getOrElse(Value, selects(Id)))
            val str = if (v.isLiteral()) v.asLiteral().getString else v.toString()
            sb append " [\n    VGraph:value \"" append str append "\" ;\n  ],"
          }
          sb append " [] ;\n"

        case "tree" ⇒
          case class Tree(id: String, value: String, var children: List[Tree])
          var trees = Map[String, Tree]()
          var owners = Map[String, List[Tree]]()
          val root = Tree("<root>", "", Nil)

          for (rs ← srs) {
            val id = rs.uri(selects(Id))
            val value = rs.string(selects(Value))
            val owner = rs.uri(selects(Rel))
            val nt = Tree(id, value, Nil)
            trees += id → nt
            owners += owner → (nt :: owners.getOrElse(owner, Nil))
          }
          for ((owner, ts) ← owners; t ← ts)
            trees.getOrElse(owner, root).children ::= t

          def print(t: Tree, indent: Int): Unit = {
            sb append " "*indent append "VGraph:value \"" append t.value append "\" ;\n"
            if (t.children.nonEmpty) {
              sb append " "*indent append "VGraph:edges "
              t.children foreach { c ⇒
                sb append "[\n"
                print(c, indent+2)
                sb append " "*indent append "], "
              }
              sb append "[] ;\n"
            }
          }

          sb append "  VResponse:graph "
          root.children foreach { c ⇒
            sb append "[\n"
            print(c, 4)
            sb append "  ], "
          }
          sb append "[] ;\n"
      }
      sb append ".\n"
      val vresp = sb.toString()
      log.info(s"Visualization response for natural language query `$query`:\n$vresp")
      vresp
    }

    handleNlQuery()
    visualization match {
      case "tree" ⇒
        findRelationshipInformation()
      case _ ⇒
    }
    mkTurtle
  }

  def headCommit(model: SparqlModel): Option[String] = {
    val rs = withQueryService(model, """
      prefix Commit:<http://amora.center/kb/amora/Schema/Commit/>
      select ?hash where {
        ?commit a Commit: .
        ?commit Commit:hash ?hash .
        filter not exists {
          ?p Commit:previous ?commit .
        }
      }
    """)
    if (rs.hasNext())
      Some(rs.next().get("hash").asLiteral().getString)
    else
      None
  }

  def headCommitUri(model: SparqlModel): Option[String] = {
    val rs = withQueryService(model, """
      prefix Commit:<http://amora.center/kb/amora/Schema/Commit/>
      select ?commit where {
        ?commit a Commit: .
        filter not exists {
          ?p Commit:previous ?commit .
        }
      }
    """)
    if (rs.hasNext())
      Some(rs.next().get("commit").toString)
    else
      None
  }

  def listCommits(model: SparqlModel): List[String] = {
    def loop(nextHash: String, hashes: List[String]): List[String] = {
      val prevHash = sparqlQuery"""
        prefix Commit:<http://amora.center/kb/amora/Schema/Commit/>
        select ?hash where {
          ?next Commit:hash "$nextHash" .
          ?next Commit:previous ?commit .
          ?commit Commit:hash ?hash .
        }
      """.runOnModel(model).map { r ⇒
        r.string("hash")
      }

      if (prevHash.isEmpty)
        hashes
      else {
        val h = prevHash.head
        loop(h, h :: hashes)
      }
    }

    val commits = headCommit(model) map { head ⇒
      loop(head, List(head)).reverse
    }
    commits.getOrElse(Nil)
  }

  def showCommit(dataset: Dataset, commit: String): SparqlModel =
    new SparqlModel(dataset.getNamedModel(commitModelName(commit)))

  def withUpdateService(model: SparqlModel, query: String)(f: ParameterizedSparqlString ⇒ Unit): Unit = {
    val pss = new ParameterizedSparqlString
    pss.setCommandText(query)
    f(pss)
    val update = pss.asUpdate()
    UpdateAction.execute(update, model.model)
  }

  def doesIdExist(model: SparqlModel, id: String): Boolean =
    runAskQuery(model, s"ASK { <$id> ?p ?o }")

  def runAskQuery(model: SparqlModel, query: String): Boolean = {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), model.model)
    qexec.execAsk()
  }

  def withConstructService(model: SparqlModel, query: String): SparqlModel = {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), model.model)
    new SparqlModel(qexec.execConstruct())
  }

  def withQueryService(model: SparqlModel, query: String): ResultSetRewindable = {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), model.model)
    ResultSetFactory.makeRewindable(qexec.execSelect())
  }

  def withSparqlService(endpoint: String, query: String): ResultSetRewindable = {
    val qe = QueryExecutionFactory.sparqlService(endpoint, query)
    ResultSetFactory.makeRewindable(qe.execSelect())
  }

  def mkInMemoryDataset: RawDataset =
    RawDataset(TDBFactory.createDataset())

  def mkDataset(location: String): RawDataset = {
    // we need to create the location if necessary because the library doesn't do it
    new java.io.File(location).mkdirs()
    RawDataset(TDBFactory.createDataset(location))
  }

  def writeDataset[A](dataset: RawDataset)(f: Dataset ⇒ A): A = {
    internalWithDataset(dataset.dataset, ReadWrite.WRITE)(f)
  }

  def readDataset[A](dataset: RawDataset)(f: Dataset ⇒ A): A = {
    internalWithDataset(dataset.dataset, ReadWrite.READ)(f)
  }

  def withModel[A](dataset: Dataset)(f: SparqlModel ⇒ A): A = {
    val model = new SparqlModel(ModelFactory.createRDFSModel(dataset.getNamedModel(modelName)))
    model.model.begin()
    try {
      val res = f(model)
      model.model.commit()
      res
    // no catch here because 'abort' on models is not supported
    } finally {
      model.model.close()
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
