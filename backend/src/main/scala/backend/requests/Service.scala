package backend.requests

import java.io.ByteArrayInputStream
import java.io.File

import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QuerySolution
import org.apache.jena.query.ResultSetFactory
import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.riot.RiotException

import backend.AkkaLogging

trait Service extends AkkaLogging {

  /**
   * Expects a request encoded in N3 and returns a response encoded in N3.
   */
  def serviceRequest(n3Request: String): String = {
    val reqModel = fillModel(ModelFactory.createDefaultModel(), n3Request)
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

    val param = serviceParam.map {
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

    run(serviceClassName, serviceMethod, param).toString()
  }

  private def run(className: String, methodName: String, param: Map[String, Param]): Any = {
    val cls = Class.forName(className)
    val obj = cls.newInstance()
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
    val fileName = s"$serviceName.service.n3"
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

  private def execQuery[A](m: Model, query: String)(f: QuerySolution ⇒ A): Seq[A] = {
    import scala.collection.JavaConverters._
    val qexec = QueryExecutionFactory.create(QueryFactory.create(query), m)
    val rs = ResultSetFactory.makeRewindable(qexec.execSelect())
    rs.asScala.map(f(_)).toSeq
  }

  private def fillModel(m: Model, str: String): Model = {
    val in = new ByteArrayInputStream(str.getBytes)
    m.read(in, null, "N3")
    m
  }

  private case class Param(name: String, tpe: Class[_], value: Any)
}
