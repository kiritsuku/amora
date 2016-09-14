package amora.backend.indexer

import java.net.URLEncoder

import org.junit.Test

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Accept
import amora.backend.CustomContentTypes
import amora.backend.schema._

class IndexerTest extends RestApiTest {
  import amora.backend.TestUtils._

  @Test
  def jsonld_context_can_be_retrieved(): Unit = {
    testReq(get("http://amora.center/kb/amora/Format/0.1/amora/Format/0.1/schema.jsonld?format=jsonld")) {
      status === StatusCodes.OK
    }
  }

  @Test
  def error_for_invalid_format(): Unit = {
    testReq(get("http://amora.center/kb/amora/Format/0.1/amora/Format/0.1/schema.jsonld?format=invalid")) {
      status === StatusCodes.BadRequest
    }
  }

  @Test
  def error_for_invalid_format_uri(): Unit = {
    testReq(get("http://amora.center/kb/amora/Format/0.1/amora/Format/0.1/invalid.jsonld?format=jsonld")) {
      status === StatusCodes.NotFound
    }
  }

  @Test
  def sparql_get_requests_are_possible(): Unit = {
    val query = "query="+URLEncoder.encode("select * where {?s ?p ?o} limit 3", "UTF-8")
    testReq(get(s"http://amora.center/sparql?$query")) {
      status === StatusCodes.OK
    }
  }

  @Test
  def sparql_get_request_misses_query_param(): Unit = {
    val query = URLEncoder.encode("select * where {?s ?p ?o} limit 3", "UTF-8")
    testReq(get(s"http://amora.center/sparql?$query")) {
      status === StatusCodes.BadRequest
    }
  }

  @Test
  def sparql_post_requests_are_possible(): Unit = {
    testReq(post("http://amora.center/sparql", "select * where {?s ?p ?o} limit 3", header = Accept(CustomContentTypes.`application/sparql-results+json`))) {
      status === StatusCodes.OK
    }
  }

  @Test
  def missing_accept_header_for_sparql_post_requests(): Unit = {
    testReq(post("http://amora.center/sparql", "select * where {?s ?p ?o} limit 3")) {
      status === StatusCodes.NotAcceptable
    }
  }

  @Test
  def encoded_sparql_post_requests_are_possible(): Unit = {
    val query = "query="+URLEncoder.encode("select * where {?s ?p ?o} limit 3", "UTF-8")
    val e = HttpEntity(CustomContentTypes.`application/x-www-form-urlencoded(UTF-8)`, query)
    testReq(post("http://amora.center/sparql", e, header = Accept(CustomContentTypes.`application/sparql-results+json`))) {
      status === StatusCodes.OK
    }
  }

  @Test
  def encoded_sparql_post_request_misses_query_param(): Unit = {
    val query = URLEncoder.encode("select * where {?s ?p ?o} limit 3", "UTF-8")
    val e = HttpEntity(CustomContentTypes.`application/x-www-form-urlencoded(UTF-8)`, query)
    testReq(post("http://amora.center/sparql", e, header = Accept(CustomContentTypes.`application/sparql-results+json`))) {
      status === StatusCodes.BadRequest
    }
  }

  @Test
  def invalid_content_type_for_sparql_post_requests(): Unit = {
    val e = HttpEntity(CustomContentTypes.`text/n3(UTF-8)`, "invalid query")
    testReq(post("http://amora.center/sparql", e, header = Accept(CustomContentTypes.`application/sparql-results+json`))) {
      status === StatusCodes.UnsupportedMediaType
    }
  }

  @Test
  def syntax_error_in_sparql_post_request(): Unit = {
    testReq(post("http://amora.center/sparql", "syntax error", header = Accept(CustomContentTypes.`application/sparql-results+json`))) {
      status === StatusCodes.InternalServerError
    }
  }

  @Test
  def add_json_post_requests_are_possible(): Unit = {
    testReq(post("http://amora.center/add-json", """
      {
        "tpe":"artifact",
        "artifacts":[
          {
            "organization":"com.freedomotic",
            "name":"hello-world",
            "version":"3.0"
          }
        ]
      }
    """)) {
      status === StatusCodes.OK
    }
    scheduleReq(post("http://amora.center/itemFinished?id=1", "")) {
      status === StatusCodes.OK
      respAsString.toBoolean
    }
    testReq((post("http://amora.center/sparql", """
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      select * where {
        [a p:] p:name ?name .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      val r = respAsResultSet()
      status === StatusCodes.OK
      r.next().get("name").asLiteral().getString === "hello-world"
    }
  }

  @Test
  def syntax_error_in_sparql_update(): Unit = {
    testReq(post("http://amora.center/sparql-update", s"syntax error")) {
      status === StatusCodes.InternalServerError
    }
  }

  @Test
  def invalid_content_type_for_sparql_update_post_requests(): Unit = {
    val e = HttpEntity(CustomContentTypes.`text/n3(UTF-8)`, "invalid query")
    testReq(post("http://amora.center/sparql-update", e)) {
      status === StatusCodes.UnsupportedMediaType
    }
  }

  @Test
  def add_single_project(): Unit = {
    val q = Schema.mkSparqlUpdate(Seq(Project("p")))
    testReq(post("http://amora.center/sparql-update", q)) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      select * where {
        [a p:] p:name ?name .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(Seq(Data("name", "p")))
    }
  }

  @Test
  def encoded_sparql_update_post_requests_are_possible(): Unit = {
    val q = Schema.mkSparqlUpdate(Seq(Project("p")))
    val query = "query="+URLEncoder.encode(q, "UTF-8")
    val e = HttpEntity(CustomContentTypes.`application/x-www-form-urlencoded(UTF-8)`, query)
    testReq(post("http://amora.center/sparql-update", e)) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      select * where {
        [a p:] p:name ?name .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(Seq(Data("name", "p")))
    }
  }

  @Test
  def encoded_sparql_update_post_request_misses_query_param(): Unit = {
    val q = Schema.mkSparqlUpdate(Seq(Project("p")))
    val query = URLEncoder.encode(q, "UTF-8")
    val e = HttpEntity(CustomContentTypes.`application/x-www-form-urlencoded(UTF-8)`, query)
    testReq(post("http://amora.center/sparql-update", e)) {
      status === StatusCodes.BadRequest
    }
  }

  @Test
  def add_multiple_projects(): Unit = {
    val q = Schema.mkSparqlUpdate(Seq(Project("p1"), Project("p2")))
    testReq(post("http://amora.center/sparql-update", q)) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      select ?name where {
        [a p:] p:name ?name .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "p1")),
          Seq(Data("name", "p2")))
    }
  }

  @Test
  def add_single_artifact(): Unit = {
    val a = Artifact(Project("p"), "o", "n", "v1")
    val q = Schema.mkSparqlUpdate(Seq(a))
    testReq(post("http://amora.center/sparql-update", q)) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      select ?organization ?name ?version where {
        [a a:] a:organization ?organization ; a:name ?name ; a:version ?version .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("organization", "o"), Data("name", "n"), Data("version", "v1")))
    }
  }

  @Test
  def add_multiple_artifacts(): Unit = {
    val p = Project("p")
    val a1 = Artifact(p, "o1", "n1", "v1")
    val a2 = Artifact(p, "o2", "n2", "v2")
    val q = Schema.mkSparqlUpdate(Seq(a1, a2))
    testReq(post("http://amora.center/sparql-update", q)) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      select ?organization ?name ?version where {
        [a a:] a:organization ?organization ; a:name ?name ; a:version ?version .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("organization", "o1"), Data("name", "n1"), Data("version", "v1")),
          Seq(Data("organization", "o2"), Data("name", "n2"), Data("version", "v2")))
    }
  }

  @Test
  def multiple_artifacts_can_belong_to_the_same_project(): Unit = {
    val p = Project("p")
    val a1 = Artifact(p, "o1", "n1", "v1")
    val a2 = Artifact(p, "o2", "n2", "v2")
    val q = Schema.mkSparqlUpdate(Seq(a1, a2))
    testReq(post("http://amora.center/sparql-update", q)) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      select distinct ?name where {
        [a a:] a:owner [p:name ?name] .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "p")))
    }
  }

  @Test
  def the_owner_of_a_project_does_not_exist(): Unit = {
    val q = Schema.mkSparqlUpdate(Seq(Project("p")))
    testReq(post("http://amora.center/sparql-update", q)) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      select ?owner where {
        [a p:] p:owner ?owner .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq()
    }
  }

  @Test
  def the_owner_of_an_artifact_is_a_project(): Unit = {
    val a = Artifact(Project("p"), "o", "n", "v1")
    val q = Schema.mkSparqlUpdate(Seq(a))
    testReq(post("http://amora.center/sparql-update", q)) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      select ?tpe where {
        [a a:] a:owner [a ?tpe] .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Project/0.1/")))
    }
  }

  @Test
  def add_single_file(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "A.scala" → """
        package pkg
      """)
    testReq((post("http://amora.center/sparql", """
      prefix f:<http://amora.center/kb/amora/Schema/0.1/File/0.1/>
      select ?name where {
        [a f:] f:name ?name .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "A.scala")))
    }
  }

  @Test
  def add_multiple_files(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "A.scala" → """
        package pkg
      """,
      "B.scala" → """
        package pkg
      """)
    testReq((post("http://amora.center/sparql", """
      prefix f:<http://amora.center/kb/amora/Schema/0.1/File/0.1/>
      select ?name where {
        [a f:] f:name ?name .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "A.scala")),
          Seq(Data("name", "B.scala")))
    }
  }

  @Test
  def files_with_same_name_can_belong_to_different_artifacts(): Unit = {
    indexData(Artifact(Project("p1"), "o1", "n1", "v1"),
      "A.scala" → """
        package pkg
      """)
    indexData(Artifact(Project("p2"), "o2", "n2", "v2"),
      "A.scala" → """
        package pkg
      """)
    testReq((post("http://amora.center/sparql", """
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
      prefix f:<http://amora.center/kb/amora/Schema/0.1/File/0.1/>
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      prefix amora:<http://amora.center/kb/amora/Schema/0.1/>
      select ?name ?version where {
        [a f:] amora:owner* [a a:; a:version ?version]; amora:name ?name .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "A.scala"), Data("version", "v1")),
          Seq(Data("name", "A.scala"), Data("version", "v2")))
    }
  }

  @Test
  def artifacts_with_same_name_can_belong_to_different_projects(): Unit = {
    val a1 = Artifact(Project("p1"), "o", "n", "v")
    val a2 = Artifact(Project("p2"), "o", "n", "v")
    val p1 = Package("pkg1", a1)
    val p2 = Package("pkg2", a2)
    val q = Schema.mkSparqlUpdate(Seq(p1, p2))
    testReq(post("http://amora.center/sparql-update", q)) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """
      prefix pkg:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      prefix amora:<http://amora.center/kb/amora/Schema/0.1/>
      select ?pname ?pkgname where {
        [a pkg:] amora:owner* [a p:; amora:name ?pname]; amora:name ?pkgname .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("pname", "p1"), Data("pkgname", "pkg1")),
          Seq(Data("pname", "p2"), Data("pkgname", "pkg2")))
    }
  }

  @Test
  def add_single_package(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "A.scala" → """
        package pkg
      """)
    testReq((post("http://amora.center/sparql", """
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
      select ?name where {
        [a p:] p:name ?name .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "pkg")))
    }
  }

  @Test
  def the_owner_of_the_top_package_is_an_artifact(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "A.scala" → """
        package pkg
      """)
    testReq((post("http://amora.center/sparql", """
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
      select ?tpe where {
        [a p:] p:owner [a ?tpe] .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/")))
    }
  }

  @Test
  def the_owner_of_a_non_top_package_is_a_package(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "A.scala" → """
        package pkg.inner
      """)
    testReq((post("http://amora.center/sparql", """
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
      select ?name ?tpe where {
        [p:owner [a p:]] p:name ?name ; a ?tpe .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "inner"), Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Package/0.1/")),
          Seq(Data("name", "inner"), Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Decl/0.1/")))
    }
  }

  @Test
  def add_single_class(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "A.scala" → """
        package pkg
        class A
      """)
    testReq((post("http://amora.center/sparql", """
      prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
      select ?name where {
        [a c:] c:name ?name .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "A")))
    }
  }

  @Test
  def the_owner_of_a_top_level_class_is_a_file(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "A.scala" → """
        package pkg
        class A
      """)
    testReq((post("http://amora.center/sparql", """
      prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
      select ?tpe where {
        [a c:] c:owner [a ?tpe] .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/File/0.1/")))
    }
  }

  @Test
  def the_owner_of_a_top_level_class_in_the_default_package_is_a_file(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "A.scala" → """
        class A
      """)
    testReq(post("http://amora.center/sparql", """
      prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
      select ?tpe where {
        [a c:] c:owner [a ?tpe] .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/File/0.1/")))
    }
  }

  @Test
  def the_owner_of_a_file_is_a_package(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "A.scala" → """
        package pkg
      """)
    testReq(post("http://amora.center/sparql", """
      prefix f:<http://amora.center/kb/amora/Schema/0.1/File/0.1/>
      select ?tpe where {
        [a f:] f:owner [a ?tpe] .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Package/0.1/")),
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Decl/0.1/")))
    }
  }

  @Test
  def the_owner_of_a_def_is_a_class(): Unit = {
    indexData(Artifact(Project("p"), "o", "n", "v1"),
      "A.scala" → """
        package pkg
        class A {
          def method = 0
        }
      """)
    testReq(post("http://amora.center/sparql", """
      prefix d:<http://amora.center/kb/amora/Schema/0.1/Def/0.1/>
      select ?tpe where {
        [a d:] d:owner [a ?tpe] .
      }
    """, header = Accept(CustomContentTypes.`application/sparql-results+json`))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Class/0.1/")),
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Decl/0.1/")))
    }
  }
}
