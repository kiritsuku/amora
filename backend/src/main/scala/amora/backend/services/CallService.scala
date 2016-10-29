package amora.backend.services

import java.io.File
import java.net.URL
import java.net.URLClassLoader

import org.apache.jena.riot.RiotException

import akka.actor.ActorSystem
import amora.backend.ActorLogger
import amora.backend.AkkaLogging
import amora.backend.Logger
import amora.backend.indexer.ArtifactFetcher
import amora.backend.indexer.ArtifactFetcher.DownloadSuccess
import amora.converter.protocol.Artifact
import amora.converter.protocol.Project

private[services] object CallService {
  val ScalaOrganization = "org.scala-lang"
  val ScalaLibrary = "scala-library"

  case class Param(name: String, tpe: Class[_], value: Any)
  case class BuildDependency(tpe: DependencyType, organization: String, name: String, version: String)
  case class Build(name: String, version: String, dependencies: Seq[BuildDependency], serviceDependencies: Seq[String])

  sealed trait DependencyType extends Product with Serializable
  case object ScalaDependency extends DependencyType
  case object MavenDependency extends DependencyType
}

class CallService(override val uri: String, override val system: ActorSystem) extends ScalaService with AkkaLogging {
  import CallService._
  import amora.api._

  def run(ttlRequest: String): String = {
    serviceRequest(ttlRequest)
  }

  private def serviceRequest(ttlRequest: String): String = {
    val reqModel = turtleModel(ttlRequest)
    val (serviceRequest, serviceId) = sparqlQuery"""
      prefix service: <http://amora.center/kb/Schema/Service/>
      select * where {
        ?request service:serviceId ?service .
      }
    """.runOnModel(reqModel).map { row ⇒
      row.uri("request") → row.uri("service")
    }.head

    val serviceModel = mkServiceModel(serviceId)
    val (serviceMethod, serviceClassName) = sparqlQuery"""
      prefix service: <http://amora.center/kb/Schema/Service/>
      select * where {
        <$serviceId> service:method [
          service:name ?name
        ] .
        <$serviceId> service:name ?className .
      }
    """.runOnModel(serviceModel).map { row ⇒
      row.string("name") → row.string("className")
    }.head

    def handleListParam(paramId: String) = {
      val name = sparqlQuery"""
        prefix service: <http://amora.center/kb/Schema/Service/>
        select * where {
          <_:$paramId> service:name ?name .
        }
      """.runOnModel(reqModel).map { row ⇒
        row.string("name")
      }.head

      val list = sparqlQuery"""
        prefix service: <http://amora.center/kb/Schema/Service/>
        prefix cu: <http://amora.center/kb/Schema/CompilationUnit/>
        select * where {
          <_:$paramId> service:value [
            cu:fileName ?fileName ;
            cu:source ?source ;
          ] .
        }
      """.runOnModel(reqModel).map { row ⇒
        row.string("fileName") → row.string("source")
      }.toList

      Map(name → Param(name, classOf[List[_]], list))
    }

    def handleLiteralParam(paramId: String) = {
      val requestParam = sparqlQuery"""
        prefix service: <http://amora.center/kb/Schema/Service/>
        select * where {
          <_:$paramId>
            service:name ?name ;
            service:value ?value ;
          .
        }
      """.runOnModel(reqModel).map { row ⇒
        row.string("name") → row.literal("value")
      }.toMap

      val serviceParam = sparqlQuery"""
        prefix service: <http://amora.center/kb/Schema/Service/>
        select * where {
          <$serviceId> service:method [
            service:param [
              service:name ?param ;
              a ?tpe ;
            ] ;
          ] .
        }
      """.runOnModel(serviceModel).map { row ⇒
        row.string("param") → row.uri("tpe")
      }.toMap

      requestParam.map {
        case (name, value) ⇒
          // TODO handle ???
          val tpe = serviceParam.getOrElse(name, ???)
          name → (tpe match {
            case "http://www.w3.org/2001/XMLSchema#integer" ⇒
              Param(name, classOf[Int], value.int)
            case "http://www.w3.org/2001/XMLSchema#string" ⇒
              Param(name, classOf[String], value.string)
          })
      }
    }

    val param = {
      val requestParam = sparqlQuery"""
        prefix service: <http://amora.center/kb/Schema/Service/>
        select ?p (isLiteral(?value) as ?isLit) where {
          <$serviceRequest> service:method/service:param ?p .
          ?p
            service:name ?name ;
            service:value ?value ;
          .
        }
      """.runOnModel(reqModel).map { row ⇒
        row.uri("p") → row.boolean("isLit")
      }.toMap
      requestParam flatMap {
        case (paramId, isLiteral) ⇒
          if (isLiteral)
            handleLiteralParam(paramId)
          else
            handleListParam(paramId)
      }
    }

    val serviceLogger = new ActorLogger()(system)

    val hasBuild = sparqlQuery"""
      prefix service: <http://amora.center/kb/Schema/Service/>
      ask {
        <$serviceId> service:build ?build .
      }
    """.askOnModel(serviceModel)

    val cl =
      if (hasBuild)
        mkBuildClassLoader(serviceModel, serviceId, serviceLogger)
      else
        getClass.getClassLoader

    val res = run(serviceLogger, cl, serviceClassName, serviceMethod, param)
    serviceLogger.close()
    res.toString()
  }

  private def mkBuildClassLoader(serviceModel: SparqlModel, serviceId: String, serviceLogger: Logger): ClassLoader = {
    val urls = mkBuildClassPath(serviceModel, serviceId, serviceLogger)
    new URLClassLoader(urls.toArray, getClass.getClassLoader)
  }

  private def mkBuildClassPath(serviceModel: SparqlModel, serviceId: String, serviceLogger: Logger): Seq[URL] = {
    val (buildName, buildVersion) = sparqlQuery"""
      prefix service:<http://amora.center/kb/Schema/Service/>
      prefix Build:<http://amora.center/kb/amora/Schema/Build/>
      select * where {
        <$serviceId> service:build [
          Build:name      ?buildName ;
          Build:version   ?buildVersion ;
        ] .
      }
    """.runOnModel(serviceModel).map { row ⇒
      row.string("buildName") → row.string("buildVersion")
    }.head

    val availableServices = registeredServices.toSet
    val serviceDeps = sparqlQuery"""
      prefix service:<http://amora.center/kb/Schema/Service/>
      prefix Build:<http://amora.center/kb/amora/Schema/Build/>
      prefix ServiceDependency:<http://amora.center/kb/amora/Schema/ServiceDependency/>
      select * where {
        <$serviceId> service:build/Build:dependency [
          a                   ServiceDependency: ;
          service:serviceId   ?id ;
        ] .
      }
    """.runOnModel(serviceModel).map { row ⇒
      row.uri("id")
    }.toList

    val notExistingServiceDeps = serviceDeps.filterNot(availableServices)
    if (notExistingServiceDeps.nonEmpty)
      throw new IllegalStateException("The following service dependencies do not exist: " + notExistingServiceDeps.mkString(", "))

    val rawDeps = sparqlQuery"""
      prefix service:<http://amora.center/kb/Schema/Service/>
      prefix Build:<http://amora.center/kb/amora/Schema/Build/>
      prefix Artifact:<http://amora.center/kb/amora/Schema/Artifact/>
      select * where {
        <$serviceId> service:build/Build:dependency [
          a                       ?tpe ;
          Artifact:organization   ?organization ;
          Artifact:name           ?name ;
          Artifact:version        ?version ;
        ] .
      }
    """.runOnModel(serviceModel).map { row ⇒
      val tpe = row.uri("tpe") match {
        case "http://amora.center/kb/amora/Schema/ScalaDependency/" =>
          ScalaDependency
        case "http://amora.center/kb/amora/Schema/MavenDependency/" =>
          MavenDependency
      }
      val organization = row.string("organization")
      val name = row.string("name")
      val version = row.string("version")
      BuildDependency(tpe, organization, name, version)
    }.toList

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
    val build = Build(buildName, buildVersion, deps, serviceDeps)
    mkClassPath(serviceModel, serviceId, serviceLogger, build)
  }

  private def mkClassPath(serviceModel: SparqlModel, serviceId: String, serviceLogger: Logger, build: Build): Seq[URL] = {
    if (build.dependencies.isEmpty && build.serviceDependencies.isEmpty)
      Nil
    else {
      val res = handleBuild(serviceLogger, build.name, build.dependencies)
      val artifactUrls = res.collect {
        case DownloadSuccess(artifact, file) ⇒
          file.toURI().toURL()
      }

      val serviceDepsUrls = build.serviceDependencies flatMap { serviceId ⇒
        mkBuildClassPath(mkServiceModel(serviceId), serviceId, serviceLogger)
      }

      val serviceOutputFolderUrls = {
        val serviceLocation = serviceFile(serviceId).getParent
        val relativeOutputFolders = sparqlQuery"""
          prefix service:<http://amora.center/kb/Schema/Service/>
          prefix Build:<http://amora.center/kb/amora/Schema/Build/>
          prefix Artifact:<http://amora.center/kb/amora/Schema/Artifact/>
          select * where {
            <$serviceId> service:build/Build:outputFolder ?out .
          }
        """.runOnModel(serviceModel).map { row ⇒
          row.uri("out")
        }.toList
        val absoluteOutputFolders = relativeOutputFolders.map { out ⇒
          val isAbsolute = out.startsWith("/")
          if (isAbsolute)
            out
          else
            serviceLocation + "/" + out
        }

        absoluteOutputFolders.map { out ⇒
          new File(out).toURI.toURL
        }
      }

      val cp = (artifactUrls ++ serviceDepsUrls ++ serviceOutputFolderUrls).distinct
      log.info(s"Use classpath for `$serviceId`:\n${cp.sortBy(_.getPath).map("  " + _ + "\n").mkString}")
      cp
    }
  }

  private def serviceFile(serviceId: String): File = {
    val r = sparqlRequest(s"""
      prefix service:<http://amora.center/kb/Schema/Service/>
      select * where {
        <$serviceId> service:path ?path .
      }
    """)
    new File(r.map(_.uri("path")).head)
  }

  private def handleBuild(serviceLogger: Logger, buildName: String, buildDependencies: Seq[BuildDependency]) = {
    val fetcher = new ArtifactFetcher with ScalaService {
      override def logger = serviceLogger
      override def cacheLocation = new File(system.settings.config.getString("app.storage.artifact-repo"))
    }
    val uriField = fetcher.getClass.getDeclaredField("uri")
    uriField.setAccessible(true)
    uriField.set(fetcher, uri)

    val artifacts = buildDependencies map { d ⇒
      Artifact(Project(buildName), d.organization, d.name, d.version)
    }
    fetcher.downloadArtifacts(artifacts)
  }

  private def run(serviceLogger: Logger, cl: ClassLoader, className: String, methodName: String, param: Map[String, Param]): Any = {
    val cls = cl.loadClass(className)
    val ctor = cls.getConstructors.head
    val hasDefaultCtor = ctor.getParameterCount == 0
    val obj =
      if (hasDefaultCtor)
        cls.newInstance()
      else {
        val ctorParam = ctor.getParameterTypes map {
          case c if c == classOf[Logger] ⇒ serviceLogger
          case c if c == classOf[ActorSystem] ⇒ system
          case c ⇒ throw new IllegalStateException(s"Constructor `$ctor` takes unknown argument type `$c`.")
        }
        ctor.newInstance(ctorParam: _*)
      }
    val uriField = cls.getDeclaredField("uri")
    uriField.setAccessible(true)
    uriField.set(obj, uri)
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
    val orderedParam = names.map { name ⇒
      param.get(name) match {
        case Some(param) ⇒ param.value.asInstanceOf[Object]
        case None ⇒ throw new IllegalStateException(s"The parameter `$name` does not exist in the request.")
      }
    }
    log.info(s"Calling service method:\n  $m")
    m.invoke(obj, orderedParam: _*)
  }

  private def registeredServices: Seq[String] = {
    sparqlRequest("""
      prefix service:<http://amora.center/kb/Schema/Service/>
      select * where {
        ?s a service: .
      }
    """).map(_.uri("s"))
  }

  private def mkServiceModel(serviceId: String) = {
    val file = serviceFile(serviceId)
    if (!file.exists())
      throw new IllegalStateException(s"Couldn't find service description file `$file`.")
    val src = io.Source.fromFile(file, "UTF-8")
    val service = src.mkString
    src.close()
    val serviceModel = try turtleModel(service) catch {
      case e: RiotException ⇒
        throw new IllegalStateException(s"Error while reading service description file `$file`: ${e.getMessage}.", e)
    }
    serviceModel
  }
}
