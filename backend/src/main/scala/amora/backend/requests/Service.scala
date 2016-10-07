package amora.backend.requests

import java.io.ByteArrayInputStream
import java.io.File
import java.net.URLClassLoader

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QuerySolution
import org.apache.jena.query.ResultSetFactory
import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.riot.RiotException

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Route
import amora.backend.ActorLogger
import amora.backend.AkkaLogging
import amora.backend.CustomContentTypes
import amora.backend.Logger
import amora.backend.indexer.ArtifactFetcher
import amora.backend.indexer.ArtifactFetcher.DownloadSuccess
import amora.backend.services.ScalaService
import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

trait Service extends Directives with AkkaLogging {
  import Service._

  private implicit val d = system.dispatcher
  private val config = system.settings.config
  private val testMode = config.getBoolean("app.test-mode")
  private val interface = config.getString("app.interface")
  private val port = config.getInt("app.port")

  /**
   * Expects a request encoded in Turtle and returns a response encoded in Turtle.
   */
  def mkServiceRequest(ttlRequest: String): Route = {
    onComplete(Future(serviceRequest(ttlRequest))) {
      case Success(resp) ⇒
        complete(HttpEntity(CustomContentTypes.`text/turtle(UTF-8)`, resp))
      case Failure(t) ⇒
        log.error(t, "Error happened while handling service request.")
        complete(HttpResponse(StatusCodes.InternalServerError, entity = s"Internal server error: ${t.getMessage}"))
    }
  }

  private def serviceRequest(ttlRequest: String): String = {
    val reqModel = fillModel(ModelFactory.createDefaultModel(), ttlRequest)
    val (serviceRequest, serviceName) = execQuery(reqModel, """
      prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      select * where {
        ?request service:serviceId ?service .
      }
    """) { qs ⇒
      qs.get("request").toString() → qs.get("service").toString()
    }.head

    val serviceModel = mkServiceModel(serviceName.split('/').last)
    val serviceParam = execQuery(serviceModel, s"""
      prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      select * where {
        <$serviceName> service:method [
          service:param [
            service:name ?param ;
            a ?tpe ;
          ] ;
        ] .
      }
    """) { qs ⇒
      val param = qs.get("param").toString()
      val tpe = qs.get("tpe").toString()
      param → tpe
    }.toMap

    val (serviceMethod, serviceClassName) = execQuery(serviceModel, s"""
      prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      select * where {
        <$serviceName> service:method [
          service:name ?name
        ] .
        <$serviceName> service:name ?className .
      }
    """) { qs ⇒
      qs.get("name").toString() → qs.get("className").asLiteral().getString
    }.head

    val isLiteral = execAsk(reqModel, s"""
      prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      ask {
        <$serviceRequest> service:method/service:param/service:value ?value .
        filter isLiteral(?value)
      }
    """)

    def complexParam = execQuery(reqModel, s"""
      prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      prefix cu: <http://amora.center/kb/Schema/0.1/CompilationUnit/0.1/>
      select * where {
        <$serviceRequest> service:method [
          service:param [
            service:name ?name ;
            service:value [
              cu:fileName ?fileName ;
              cu:source ?source ;
            ] ;
          ] ;
        ] .
      }
    """) { qs ⇒
      val name = qs.get("name").toString()
      val fileName = qs.get("fileName").asLiteral().getString
      val source = qs.get("source").asLiteral().getString

      name → Param(name, classOf[List[_]], List(fileName → source))
    }.toMap

    def literalParam = {
      val requestParam = execQuery(reqModel, s"""
        prefix service: <http://amora.center/kb/Schema/Service/0.1/>
        select * where {
          <$serviceRequest> service:method [
            service:param [
              service:name ?name ;
              service:value ?value ;
            ] ;
          ] .
        }
      """) { qs ⇒
        qs.get("name").toString() → qs.get("value").asLiteral()
      }.toMap

      serviceParam.map {
        case (name, tpe) ⇒
          // TODO handle ???
          val value = requestParam.getOrElse(name, ???)
          name → (tpe match {
            case "http://www.w3.org/2001/XMLSchema#integer" ⇒
              Param(name, classOf[Int], value.getInt)
            case "http://www.w3.org/2001/XMLSchema#string" ⇒
              Param(name, classOf[String], value.getString)
          })
      }
    }

    val param =
      if (isLiteral)
        literalParam
      else
        complexParam

    val serviceLogger = new ActorLogger()(system)

    def mkBuildClassLoader() = {
      val (buildName, buildVersion) = execQuery(serviceModel, s"""
        prefix service:<http://amora.center/kb/Schema/Service/0.1/>
        prefix Build:<http://amora.center/kb/amora/Schema/0.1/Build/0.1/>
        select * where {
          <$serviceName> service:build [
            Build:name ?buildName ;
            Build:version ?buildVersion ;
          ] .
        }
      """) { qs ⇒
        val buildName = qs.get("buildName").asLiteral().getString
        val buildVersion = qs.get("buildVersion").asLiteral().getString
        buildName -> buildVersion
      }.head

      val rawDeps = execQuery(serviceModel, s"""
        prefix service:<http://amora.center/kb/Schema/Service/0.1/>
        prefix Build:<http://amora.center/kb/amora/Schema/0.1/Build/0.1/>
        prefix Artifact:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
        select * where {
          <$serviceName> service:build [
            Build:dependency [
              a                       ?tpe ;
              Artifact:organization   ?organization ;
              Artifact:name           ?name ;
              Artifact:version        ?version ;
            ] ;
          ] .
        }
      """) { qs ⇒
        val tpe = qs.get("tpe").toString() match {
          case "http://amora.center/kb/amora/Schema/0.1/ScalaDependency/0.1/" =>
            ScalaDependency
          case "http://amora.center/kb/amora/Schema/0.1/MavenDependency/0.1/" =>
            MavenDependency
        }
        val organization = qs.get("organization").asLiteral().getString
        val name = qs.get("name").asLiteral().getString
        val version = qs.get("version").asLiteral().getString
        BuildDependency(tpe, organization, name, version)
      }

      // TODO what to do if no Scala library is defined?
      val scalaDep = rawDeps.find(d => d.organization == ScalaOrganization && d.name == ScalaLibrary).getOrElse(???)
      val scalaPrefix = scalaDep match {
        case d if d.version startsWith "2.11" ⇒ "_2.11"
      }

      val deps = rawDeps map { d ⇒
        if (d.tpe == ScalaDependency)
          d.copy(name = d.name + scalaPrefix)
        else
          d
      }
      val build = Build(buildName, buildVersion, deps)
      mkClassloader(serviceLogger, build)
    }

    val hasBuild = execAsk(reqModel, s"""
      prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      ask {
        <$serviceRequest> service:build ?build .
      }
    """)

    val cl =
      if (hasBuild)
        mkBuildClassLoader()
      else
        getClass.getClassLoader
    val res = run(serviceLogger, cl, serviceClassName, serviceMethod, param)
    serviceLogger.close()
    if (serviceLogger.sw.getBuffer.length() != 0)
      log.info(serviceLogger.sw.toString())
    res.toString()
  }

  private def mkClassloader(serviceLogger: Logger, build: Build): ClassLoader = {
    if (build.dependencies.isEmpty)
      getClass.getClassLoader
    else {
      val res = handleBuild(serviceLogger, build)
      val urls = res.collect {
        case DownloadSuccess(artifact, file) ⇒
          file.toURI().toURL()
      }.distinct.toArray

      log.info(s"Create classpath:\n${urls.sortBy(_.getPath).mkString("\n")}")
      new URLClassLoader(urls, getClass.getClassLoader)
    }
  }

  private def handleBuild(serviceLogger: Logger, build: Build) = {
    val fetcher = new ArtifactFetcher with ScalaService {
      override def logger = serviceLogger
      override def cacheLocation = new File(system.settings.config.getString("app.storage.artifact-repo"))
    }
    val uriField = fetcher.getClass.getDeclaredField("uri")
    uriField.setAccessible(true)
    uriField.set(fetcher, amoraUri)

    val artifacts = build.dependencies map { d ⇒
      Artifact(Project(build.name), d.organization, d.name, d.version)
    }
    fetcher.handleArtifacts(artifacts)
  }

  private def amoraUri = if (testMode) s"http://$interface:$port" else "http://amora.center"

  private def run(serviceLogger: Logger, cl: ClassLoader, className: String, methodName: String, param: Map[String, Param]): Any = {
    val cls = cl.loadClass(className)
    val ctors = cls.getConstructors
    val hasDefaultCtor = ctors.exists(_.getParameterCount == 0)
    val obj =
      if (hasDefaultCtor)
        cls.newInstance()
      else {
        // TODO handle ???
        val ctor = ctors.find(_.getParameterTypes sameElements Array(classOf[Logger])).getOrElse(???)
        ctor.newInstance(serviceLogger)
      }
    val uriField = cls.getDeclaredField("uri")
    uriField.setAccessible(true)
    uriField.set(obj, amoraUri)
    // TODO get rid of the ??? here
    val m = cls.getMethods.find(_.getName == methodName).getOrElse(???)
    val hasNoJavaParameterNames = m.getParameters.headOption.exists(_.getName == "arg0")
    val names =
      if (hasNoJavaParameterNames) {
        import scala.reflect.runtime.universe
        val mirror = universe.runtimeMirror(getClass.getClassLoader)
        // TODO what to do if encodedName is also arg0?
        mirror.reflect(obj).symbol.typeSignature.member(universe.TermName(methodName)).asMethod.paramLists.flatten.map(_.name.encodedName.toString).toList
      } else
        m.getParameters.map(_.getName).toList
    val orderedParam = names.map(name ⇒ param(name).value.asInstanceOf[Object])
    m.invoke(obj, orderedParam: _*)
  }

  private def mkServiceModel(serviceName: String): Model = {
    val cl = getClass.getClassLoader
    val fileName = s"$serviceName.service.ttl"
    val serviceFile = new File(cl.getResource(".").getPath + fileName)
    if (!serviceFile.exists())
      throw new IllegalStateException(s"Couldn't find service description file `$serviceFile`.")
    val src = io.Source.fromFile(serviceFile, "UTF-8")
    val service = src.mkString
    src.close()
    val serviceModel = try fillModel(ModelFactory.createDefaultModel(), service) catch {
      case e: RiotException ⇒
        throw new IllegalStateException(s"Error while reading service description file `$serviceFile`: ${e.getMessage}.", e)
    }
    serviceModel
  }

  private def execAsk(m: Model, query: String): Boolean = {
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), m)
    qexec.execAsk()
  }

  private def execQuery[A](m: Model, query: String)(f: QuerySolution ⇒ A): Seq[A] = {
    import scala.collection.JavaConverters._
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), m)
    val rs = ResultSetFactory.makeRewindable(qexec.execSelect())
    rs.asScala.map(f(_)).toSeq
  }

  private def fillModel(m: Model, str: String): Model = {
    val in = new ByteArrayInputStream(str.getBytes)
    m.read(in, null, "TURTLE")
    m
  }
}
private[requests] object Service {
  val ScalaOrganization = "org.scala-lang"
  val ScalaLibrary = "scala-library"

  case class Param(name: String, tpe: Class[_], value: Any)
  case class BuildDependency(tpe: DependencyType, organization: String, name: String, version: String)
  case class Build(name: String, version: String, dependencies: Seq[BuildDependency])

  sealed trait DependencyType
  case object ScalaDependency extends DependencyType
  case object MavenDependency extends DependencyType
}
