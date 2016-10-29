package amora.frontend.webui

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success

import org.scalajs.dom
import org.scalajs.dom.raw.Event
import org.scalajs.dom.raw.XMLHttpRequest

trait Requests {

  case class Range(start: Int, end: Int)

  def indexScalaSrc(src: String): Future[Unit] = {
    val ttlResp = serviceRequest(s"""
      @prefix service:<http://amora.center/kb/Schema/Service/> .
      @prefix registry:<http://amora.center/kb/Service/> .
      @prefix request:<http://amora.center/kb/ServiceRequest/> .
      @prefix cu:<http://amora.center/kb/Schema/CompilationUnit/> .
      <#this>
        a request: ;
        service:serviceId registry:ScalaSourceIndexer ;
        service:method [
          service:name "run" ;
          service:param [
            service:name "data" ;
            service:value [
              cu:fileName "x.scala" ;
              cu:source "${src.replace("\n", "\\n").replace("\"", "\\\"")}" ;
            ] ;
          ] ;
        ] ;
      .
    """)
    ttlResp.map(_ ⇒ ())
  }

  def findUsages(offset: Int): Future[Seq[Range]] = {
    val ttlResp = serviceRequest(s"""
      @prefix service:<http://amora.center/kb/Schema/Service/> .
      @prefix registry:<http://amora.center/kb/Service/> .
      @prefix request:<http://amora.center/kb/ServiceRequest/> .
      <#this>
        a request: ;
        service:serviceId registry:FindUsages ;
        service:method [
          service:name "run" ;
          service:param [
            service:name "offset" ;
            service:value $offset ;
          ] ;
        ] ;
      .
    """)

    val model = ttlResp flatMap { ttlResp ⇒
      modelAsData(ttlResp, """
        prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        prefix service:<http://amora.center/kb/Schema/Service/>
        prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
        select ?start ?end where {
          ?s service:result ?r .
          ?r decl:posStart ?start ; decl:posEnd ?end .
        }
        order by ?start ?end
      """)
    }

    model map { model ⇒
      val res = for (elem ← model.asInstanceOf[js.Array[js.Any]]) yield {
        val start = elem.jsg.start.value.toString.toInt
        val end = elem.jsg.end.value.toString.toInt
        Range(start, end)
      }
      res.toList
    }
  }

  def findDeclaration(offset: Int): Future[Option[Range]] = {
    val ttlResp = serviceRequest(s"""
      @prefix service:<http://amora.center/kb/Schema/Service/> .
      @prefix registry:<http://amora.center/kb/Service/> .
      @prefix request:<http://amora.center/kb/ServiceRequest/> .
      <#this>
        a request: ;
        service:serviceId registry:FindDeclaration ;
        service:method [
          service:name "run" ;
          service:param [
            service:name "offset" ;
            service:value $offset ;
          ] ;
        ] ;
      .
    """)

    val model = ttlResp flatMap { ttlResp ⇒
      modelAsData(ttlResp, """
        prefix service:<http://amora.center/kb/Schema/Service/>
        prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
        select ?start ?end where {
          ?s service:result ?r .
          ?r decl:posStart ?start ; decl:posEnd ?end .
        }
      """)
    }

    model map { model ⇒
      val arr = model.asInstanceOf[js.Array[js.Any]]
      if (arr.isEmpty)
        None
      else {
        val start = arr(0).jsg.start.value.toString.toInt
        val end = arr(0).jsg.end.value.toString.toInt
        Some(Range(start, end))
      }
    }
  }

  def modelAsData(ttlModel: String, query: String): Future[js.Any] = {
    val p = Promise[js.Any]

    def handleErr(err: js.Any, msg: String)(onSuccess: ⇒ Unit): Unit = {
      if (err == null)
        onSuccess
      else
        p.failure(new IllegalStateException(s"$msg\n$err"))
    }

    jsg.Bundle.rdfstore.create(f2 { (err, store) ⇒
      handleErr(err, "Error occurred while loading rdfstore.") {
        store.jsg.load("text/turtle", ttlModel, f2 { (err, loadedTriples) ⇒
          handleErr(err, "Error occurred while loading TTL data.") {
            // we can't inline `?r`, see https://github.com/antoniogarrote/rdfstore-js/issues/141
            store.jsg.execute(query, f2 { (err, graph) ⇒
              handleErr(err, "Error occurred while executing SPARQL query.") {
                p.success(graph)
              }
            })
          }
        })
      }
    })
    p.future
  }

  /**
   * Sends a SPARQL request. The response is encoded in
   * `application/sparql-results+json`.
   */
  def sparqlRequest(query: String): Future[String] = {
    val p = Promise[String]
    val r = new XMLHttpRequest
    r.open("POST", "http://amora.center/sparql", async = true)
    r.setRequestHeader("Content-type", "application/sparql-query")
    r.setRequestHeader("Accept", "application/sparql-results+json")
    r.setRequestHeader("Charset", "UTF-8")
    r.onreadystatechange = (e: Event) ⇒ {
      if (r.readyState == XMLHttpRequest.DONE) {
        if (r.status == 200)
          p.success(r.responseText)
        else
          p.failure(new IllegalStateException(s"Server responded with an error to SPARQL request.\nRequest: $query\nResponse (error code: ${r.status}): ${r.responseText}"))
      }
    }
    r.send(query)
    p.future
  }

  def onSuccess[A](fut: Future[A])(f: A ⇒ Unit): Unit = {
    fut onComplete {
      case Success(s) ⇒
        f(s)
      case Failure(f) ⇒
        dom.console.error(f.getMessage)
    }
  }

  /**
   * Sends a service request, which needs to be encoded in `text/turtle`. The
   * response is also encoded in `text/turtle`.
   */
  def serviceRequest(ttlReq: String): Future[String] = {
    val p = Promise[String]
    val r = new XMLHttpRequest
    r.open("POST", "http://amora.center/service", async = true)
    r.setRequestHeader("Content-type", "text/turtle")
    r.setRequestHeader("Accept", "text/turtle")
    r.setRequestHeader("Charset", "UTF-8")
    r.onreadystatechange = (e: Event) ⇒ {
      if (r.readyState == XMLHttpRequest.DONE) {
        if (r.status == 200)
          p.success(r.responseText)
        else
          p.failure(new IllegalStateException(s"Server responded with an error to service request.\nRequest: $ttlReq\nResponse (error code: ${r.status}): ${r.responseText}"))
      }
    }
    r.send(ttlReq)
    p.future
  }
}
