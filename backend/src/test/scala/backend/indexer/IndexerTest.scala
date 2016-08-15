package backend.indexer

import org.junit.Test

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Accept
import backend.CustomContentTypes
import backend.schema._

class IndexerTest extends RestApiTest {
  import backend.TestUtils._

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
  def sparql_post_requests_are_possible(): Unit = {
    testReq(post("http://amora.center/sparql", "query=select * where {?s ?p ?o} limit 3", header = Accept(CustomContentTypes.`sparql-results+json`))) {
      status === StatusCodes.OK
    }
  }

  @Test
  def syntax_error_in_sparql_post_request(): Unit = {
    testReq(post("http://amora.center/sparql", "query=syntax error", header = Accept(CustomContentTypes.`sparql-results+json`))) {
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
    testReq((post("http://amora.center/sparql", """query=
      prefix kb:<http://amora.center/kb/>
      select * where {
        [a kb:Project] kb:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      val r = respAsResultSet()
      status === StatusCodes.OK
      r.next().get("name").asLiteral().getString === "hello-world"
    }
  }

  @Test
  def syntax_error_in_sparql_update(): Unit = {
    testReq(post("http://amora.center/sparql-update", s"query=syntax error")) {
      status === StatusCodes.InternalServerError
    }
  }

  @Test
  def add_single_project(): Unit = {
    val q = Schema.mkSparqlUpdate(Seq(Project("p")))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      select * where {
        [a p:] p:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(Seq(Data("name", "p")))
    }
  }

  @Test
  def add_multiple_projects(): Unit = {
    val q = Schema.mkSparqlUpdate(Seq(Project("p1"), Project("p2")))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      select ?name where {
        [a p:] p:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
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
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      select ?organization ?name ?version where {
        [a a:] a:organization ?organization ; a:name ?name ; a:version ?version .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
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
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      select ?organization ?name ?version where {
        [a a:] a:organization ?organization ; a:name ?name ; a:version ?version .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
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
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      select distinct ?name where {
        [a a:] a:owner [p:name ?name] .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "p")))
    }
  }

  @Test
  def the_owner_of_a_project_does_not_exist(): Unit = {
    val q = Schema.mkSparqlUpdate(Seq(Project("p")))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      select ?owner where {
        [a p:] p:owner ?owner .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq()
    }
  }

  @Test
  def the_owner_of_an_artifact_is_a_project(): Unit = {
    val a = Artifact(Project("p"), "o", "n", "v1")
    val q = Schema.mkSparqlUpdate(Seq(a))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      select ?tpe where {
        [a a:] a:owner [a ?tpe] .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Project/0.1/")))
    }
  }

  @Test
  def add_single_file(): Unit = {
    val f = File(Package("pkg", Artifact(Project("p"), "o", "n", "v1")), "pkg/A.scala")
    val q = Schema.mkSparqlUpdate(Seq(f))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix f:<http://amora.center/kb/amora/Schema/0.1/File/0.1/>
      select ?name where {
        [a f:] f:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "pkg/A.scala")))
    }
  }

  @Test
  def add_multiple_files(): Unit = {
    val p = Package("pkg", Artifact(Project("p"), "o", "n", "v1"))
    val f1 = File(p, "pkg/A.scala")
    val f2 = File(p, "pkg/B.scala")
    val q = Schema.mkSparqlUpdate(Seq(f1, f2))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix f:<http://amora.center/kb/amora/Schema/0.1/File/0.1/>
      select ?name where {
        [a f:] f:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "pkg/A.scala")),
          Seq(Data("name", "pkg/B.scala")))
    }
  }

  @Test
  def files_with_same_name_can_belong_to_different_artifacts(): Unit = {
    val a1 = Package("pkg", Artifact(Project("p1"), "o1", "n1", "v1"))
    val a2 = Package("pkg", Artifact(Project("p2"), "o2", "n2", "v2"))
    val f1 = File(a1, "pkg/A.scala")
    val f2 = File(a2, "pkg/A.scala")
    val q = Schema.mkSparqlUpdate(Seq(f1, f2))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
      prefix f:<http://amora.center/kb/amora/Schema/0.1/File/0.1/>
      prefix a:<http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/>
      prefix amora:<http://amora.center/kb/amora/Schema/0.1/>
      select ?name ?version where {
        [a f:] amora:owner* [a a:; a:version ?version]; amora:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "pkg/A.scala"), Data("version", "v1")),
          Seq(Data("name", "pkg/A.scala"), Data("version", "v2")))
    }
  }

  @Test
  def artifacts_with_same_name_can_belong_to_different_projects(): Unit = {
    val a1 = Artifact(Project("p1"), "o", "n", "v")
    val a2 = Artifact(Project("p2"), "o", "n", "v")
    val p1 = Package("pkg1", a1)
    val p2 = Package("pkg2", a2)
    val q = Schema.mkSparqlUpdate(Seq(p1, p2))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix pkg:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Project/0.1/>
      prefix amora:<http://amora.center/kb/amora/Schema/0.1/>
      select ?pname ?pkgname where {
        [a pkg:] amora:owner* [a p:; amora:name ?pname]; amora:name ?pkgname .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("pname", "p1"), Data("pkgname", "pkg1")),
          Seq(Data("pname", "p2"), Data("pkgname", "pkg2")))
    }
  }

  @Test
  def add_single_package(): Unit = {
    val p = Package("pkg", Artifact(Project("p"), "o", "n", "v1"))
    val q = Schema.mkSparqlUpdate(Seq(p))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
      select ?name where {
        [a p:] p:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "pkg")))
    }
  }

  @Test
  def the_owner_of_the_top_package_is_an_artifact(): Unit = {
    val p = Package("pkg", Artifact(Project("p"), "o", "n", "v1"))
    val q = Schema.mkSparqlUpdate(Seq(p))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
      select ?tpe where {
        [a p:] p:owner [a ?tpe] .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Artifact/0.1/")))
    }
  }

  @Test
  def the_owner_of_a_non_top_package_is_a_package(): Unit = {
    val a = Artifact(Project("p"), "o", "n", "v1")
    val p = Package("inner", Package("pkg", a))
    val q = Schema.mkSparqlUpdate(Seq(p))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix p:<http://amora.center/kb/amora/Schema/0.1/Package/0.1/>
      select ?name ?tpe where {
        [p:owner [a p:]] p:name ?name ; a ?tpe .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "inner"), Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Package/0.1/")))
    }
  }

  @Test
  def add_single_class(): Unit = {
    val a = Artifact(Project("p"), "o", "n", "v1")
    val c = Class("A", File(Package("pkg", a), "pkg/A.scala"))
    val q = Schema.mkSparqlUpdate(Seq(c))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
      select ?name where {
        [a c:] c:name ?name .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("name", "A")))
    }
  }

  @Test
  def the_owner_of_a_top_level_class_is_a_file(): Unit = {
    val a = Artifact(Project("p"), "o", "n", "v1")
    val c = Class("A", File(a, "A.scala"))
    val q = Schema.mkSparqlUpdate(Seq(c))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix c:<http://amora.center/kb/amora/Schema/0.1/Class/0.1/>
      select ?tpe where {
        [a c:] c:owner [a ?tpe] .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/File/0.1/")))
    }
  }

  @Test
  def the_owner_of_a_file_is_a_package(): Unit = {
    val f = File(Package("pkg", Artifact(Project("p"), "o", "n", "v1")), "pkg/A.scala")
    val q = Schema.mkSparqlUpdate(Seq(f))
    testReq(post("http://amora.center/sparql-update", s"query=$q")) {
      status === StatusCodes.OK
    }
    testReq((post("http://amora.center/sparql", """query=
      prefix f:<http://amora.center/kb/amora/Schema/0.1/File/0.1/>
      select ?tpe where {
        [a f:] f:owner [a ?tpe] .
      }
    """, header = Accept(CustomContentTypes.`sparql-results+json`)))) {
      status === StatusCodes.OK
      resultSetAsData(respAsResultSet()) === Seq(
          Seq(Data("tpe", "http://amora.center/kb/amora/Schema/0.1/Package/0.1/")))
    }
  }
}
