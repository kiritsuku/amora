package backend.indexer

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
import research.converter.protocol._
import backend.actors.IndexerMessage._
import spray.json._
import backend.Content

object Indexer extends backend.Log4jLogging {

  /**
   * On startup of the indexer, we want to index some predefined data like
   * schema definitions.
   */
  def startupIndexer() = {
    val res = withDataset(IndexerConstants.IndexDataset) { dataset ⇒
      withModel(dataset, Content.ModelName) { model ⇒
        import java.io.File
        val cl = getClass.getClassLoader
        val resourceDir = new java.io.File(cl.getResource(".").getPath)
        val indexableFiles = resourceDir.listFiles().filter(_.getName.endsWith(".schema.jsonld"))

        def genJson(file: File) = {
          val src = io.Source.fromFile(file, "UTF-8")
          val jsonString = src.mkString
          val schemaName = file.getName.dropRight(".schema.jsonld".length)
          src.close()
          new SchemaGenerator().generate(schemaName, jsonString)
        }

        indexableFiles foreach { f ⇒
          val jsonString = genJson(f)
          addJsonLd(model, jsonString)
          log.info(s"Schema file `$f` successfully indexed.")
        }
      }
    }.flatten
    res match {
      case util.Success(_) ⇒
        log.info("Indexer successfully started.")
      case util.Failure(f) ⇒
        throw new RuntimeException("An error happened during initialization of the indexer.", f)
    }
  }

  def queryResultAsString(modelName: String, query: String, model: Model): Try[String] = {
    withQueryService(modelName, query)(model) map { r ⇒
      val s = new ByteArrayOutputStream

      ResultSetFormatter.out(s, r)
      new String(s.toByteArray(), "UTF-8")
    }
  }

  def flattenedQueryResult[A](modelName: String, query: String, model: Model)(f: (String, QuerySolution) ⇒ A): Try[Seq[A]] = {
    import scala.collection.JavaConverters._
    withQueryService(modelName, query)(model) map { r ⇒
      val vars = r.getResultVars.asScala.toSeq

      for { q ← r.asScala.toSeq; v ← vars } yield f(v, q)
    }
  }

  def queryResult[A](modelName: String, query: String, model: Model)(f: (String, QuerySolution) ⇒ A): Try[Seq[Seq[A]]] = {
    import scala.collection.JavaConverters._
    withQueryService(modelName, query)(model) map { r ⇒
      val vars = r.getResultVars.asScala.toSeq

      for (q ← r.asScala.toSeq) yield
        for (v ← vars) yield
          f(v, q)
    }
  }

  private def context(modelName: String) = JsObject(
      "c" → JsString(modelName),
      "s" → JsString("http://schema.org/"),
      "c:declaration" → JsObject(
        "@id" → JsString("c:declaration"),
        "@type" → JsString("@id")
      ),
      "c:owner" → JsObject(
        "@id" → JsString("c:owner"),
        "@type" → JsString("@id")
      ),
      "c:attachment" → JsObject(
        "@id" → JsString("c:attachment")
      ),
      "c:reference" → JsObject(
        "@id" → JsString("c:reference"),
        "@type" → JsString("@id")
      ),
      "c:start" → JsObject(
        "@id" → JsString("c:start")
      ),
      "c:end" → JsObject(
        "@id" → JsString("c:end")
      ),
      "c:project" → JsObject(
        "@id" → JsString("c:project"),
        "@type" → JsString("@id")
      ),
      "c:artifact" → JsObject(
        "@id" → JsString("c:artifact"),
        "@type" → JsString("@id")
      ),
      "c:organization" → JsObject(
        "@id" → JsString("c:organization")
      ),
      "c:version" → JsObject(
        "@id" → JsString("c:version")
      ),
      "c:name" → JsObject(
        "@id" → JsString("c:name")
      )
  )

  private def attachments(h: Hierarchy) =
    if (h.attachments.isEmpty)
      JsArray()
    else {
      JsArray(h.attachments.map(x ⇒ JsString(x.asString)).toVector)
    }

  private def position(pos: Position) = pos match {
    case RangePosition(start, end) ⇒
      Seq("c:start" → JsNumber(start), "c:end" → JsNumber(end))
    case _ ⇒
      Nil
  }

  private def uniqueRef(pos: Position) = pos match {
    case RangePosition(start, _) ⇒
      s"/$start"
    case _ ⇒
      ""
  }

  private def encode(str: String): String =
    URLEncoder.encode(str, "UTF-8")

  private def mkPath(file: File, decl: Decl) = {
    val Decl(name, owner) = decl
      val n = encode(name)
      val ownerPath = owner match {
        case Root ⇒
          ""
        case _: Decl ⇒
          encode(owner.asString).replace('.', '/')
        case _: Ref ⇒
          val path = encode(owner.asString).replace('.', '/')
          val f = encode(file.name)
          val h = uniqueRef(owner.position)
          s"$path/$f$h"
      }
      val sig = decl.attachments.collectFirst {
        case Attachment.JvmSignature(signature) ⇒ encode(signature)
      }.getOrElse("")
      val paramAtt = encode(decl.attachments.collectFirst {
        case Attachment.Param ⇒ "<param>"
        case Attachment.TypeParam ⇒ "<tparam>"
      }.getOrElse(""))
      val origin = file.origin match {
        case a: Artifact ⇒
          if (ownerPath.isEmpty)
            pathOf(a)
          else
            pathOf(a) + "/" + ownerPath
        case NoOrigin ⇒
          ownerPath
      }
      val fullOwnerPath =
        if (decl.attachments(Attachment.Package))
          if (decl.owner == Root)
            file.origin match {
              case NoOrigin ⇒ None
              case origin ⇒ Some(pathOf(origin))
            }
          else
            Some(origin)
        else if (decl.owner.attachments(Attachment.Package) || decl.owner == Root)
          Some(pathOf(file))
        else
          Some(origin)
      val originPath = if (origin.isEmpty) "" else origin + "/"
      val fullPath = s"$originPath$paramAtt$n$sig"
      fullOwnerPath → fullPath
  }

  private def mkModel(projectFile: File)(h: Hierarchy): Vector[JsValue] = h match {
    case Root ⇒
      Vector(JsArray())

    case decl @ Decl(name, parent) ⇒
      val tpe = decl.attachments.collectFirst {
        case Attachment.Class ⇒ "Class"
        case Attachment.Package ⇒ "Package"
        case Attachment.Def ⇒ "Def"
      }.getOrElse("Declaration")
      val (ownerPath, declPath) = mkPath(projectFile, decl)
      var classEntry = Map(
        "@id" → JsString(s"c:$declPath"),
        "@type" → JsString(s"c:$tpe"),
        "s:name" → JsString(name),
        "c:attachment" → attachments(decl),
        "c:tpe" → JsString("decl")
      )
      position(decl.position) foreach (classEntry += _)
      ownerPath foreach (p ⇒ classEntry += "c:owner" → JsString(s"c:$p"))
      val declEntry = mkModel(projectFile)(parent)
      JsObject(classEntry) +: declEntry

    case ref @ Ref(name, refToDecl, owner, qualifier) ⇒
      // TODO do not use replace function here, it is not safe since Scala
      // identifiers can contain dots.
      val path = encode(refToDecl.asString).replace('.', '/')
      val f = encode(projectFile.name)
      val h = uniqueRef(ref.position)
      val origin = projectFile.origin match {
        case a: Artifact ⇒ pathOf(a) + "/"
        case NoOrigin ⇒ ""
      }
      val fullPath = s"$origin$path/$f$h"
      var m = Map(
        "@id" → JsString(s"c:$fullPath"),
        "@type" → JsString("s:Text"),
        "c:tpe" → JsString("ref"),
        "c:attachment" → attachments(ref),
        "s:name" → JsString(ref.name),
        "c:reference" → JsString(s"c:$path")
      )
      if (owner != Root) {
        val u = encode(owner.asString).replace('.', '/')
        m += "c:owner" → JsString(s"c:$u")
      }
      position(ref.position) foreach (m += _)
      Vector(JsObject(m))
  }

  def add(modelName: String, model: Model, data: Indexable): Unit = {
    data match {
      case project: Project ⇒ addProject(modelName, project)(model).get
      case artifact: Artifact ⇒ addArtifact(modelName, artifact)(model).get
      case file: File ⇒ addFile(modelName, file)(model).get
      case _ ⇒ throw new UnsupportedOperationException(s"${data.getClass} can't be indexed.")
    }
  }

  def addProject(modelName: String, project: Project)(model: Model): Try[Unit] = Try {
    val projectEntry = Map(
      "@id" → JsString(s"c:${pathOf(project)}"),
      "@type" → JsString("c:Project"),
      "c:name" → JsString(project.name)
    )
    val contextEntry = Map(
      "@context" → context(modelName),
      "@graph" → JsArray(Vector(JsObject(projectEntry)))
    )

    addJsonLd(model, JsObject(contextEntry))
  }

  def addArtifact(modelName: String, artifact: Artifact)(model: Model): Try[Unit] = Try {
    val projectEntry = Map(
      "@id" → JsString(s"c:${pathOf(artifact.project)}"),
      "@type" → JsString("c:Project"),
      "c:name" → JsString(artifact.project.name)
    )

    val artifactEntry = Map(
      "@id" → JsString(s"c:${pathOf(artifact)}"),
      "@type" → JsString("c:Artifact"),
      "c:organization" → JsString(artifact.organization),
      "c:name" → JsString(artifact.name),
      "c:version" → JsString(artifact.version),
      "c:owner" → JsString(s"c:${pathOf(artifact.project)}")
    )

    val contextEntry = Map(
      "@context" → context(modelName),
      "@graph" → JsArray(Vector(JsObject(projectEntry), JsObject(artifactEntry)))
    )

    addJsonLd(model, JsObject(contextEntry))
  }

  def addFile(modelName: String, projectFile: File)(model: Model): Try[Unit] = Try {
    val pkg = projectFile.data.headOption.flatMap { h ⇒
      def isTopLevelDecl = h.attachments.exists(Set(Attachment.Class, Attachment.Trait, Attachment.Object))
      def isPkg = h.owner.attachments(Attachment.Package)
      if (isTopLevelDecl && isPkg && h.owner.isInstanceOf[Decl]) {
        val (_, declPath) = mkPath(projectFile, h.owner.asInstanceOf[Decl])
        Some(declPath)
      }
      else
        None
    }

    var fileEntry = Map(
      "@id" → JsString(s"c:${pathOf(projectFile)}"),
      "@type" → JsString("c:File"),
      "c:name" → JsString(projectFile.name)
    )
    pkg foreach (pkg ⇒ fileEntry += "c:owner" → JsString(s"c:$pkg"))

    val modelEntries: Vector[JsValue] = projectFile.data.flatMap(mkModel(projectFile))(collection.breakOut)

    val contextEntry = Map(
      "@context" → context(modelName),
      "@graph" → JsArray(JsObject(fileEntry) +: modelEntries)
    )

    addJsonLd(model, JsObject(contextEntry))
  }

  private def pathOf(data: Indexable): String = data match {
    case Artifact(_, organization, name, version) ⇒
      val o = encode(organization)
      val n = encode(name)
      val v = encode(version)
      s"artifact/$o/$n/$v"
    case Project(name) ⇒
      val n = encode(name)
      s"project/$n"
    case File(origin, name, _) ⇒
      val fn = name.split('/').map(encode).mkString("/")
      origin match {
        case Artifact(_, organization, name, version) ⇒
          val o = encode(organization)
          val n = encode(name)
          val v = encode(version)
          s"file/$o/$n/$v/$fn"
        case NoOrigin ⇒
          s"file/$fn"
      }
      case _ ⇒ throw new UnsupportedOperationException(s"${data.getClass} does not have a path.")
  }

  private def addJsonLd(model: Model, data: JsValue) = {
    val str = data.prettyPrint
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
