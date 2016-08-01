package backend.indexer

import org.junit.Test

import backend.TestUtils
import backend.actors.IndexerMessage._
import research.converter.protocol._

class ModelIndexerTest {
  import TestUtils._

  case class Data(varName: String, value: String)

  def ask(modelName: String, rawQuery: String, data: Indexable*): Seq[Seq[Data]] = {
    val indexer = new Indexer
    val dataset = indexer.mkInMemoryDataset
    val query = rawQuery.replaceFirst("""\?MODEL\?""", modelName)
    val res = indexer.writeDataset(dataset) { dataset ⇒
      indexer.withModel(dataset, modelName) { model ⇒
        data foreach (indexer.add(modelName, model, _))

        if (debugTests) {
          println(indexer.queryResultAsString(modelName, "select * { ?s ?p ?o }", model))
          println(indexer.queryResultAsString(modelName, query, model))
        }

        indexer.queryResult(modelName, query, model) { (v, q) ⇒
          val res = q.get(v)
          require(res != null, s"The variable `$v` does not exist in the result set.")
          Data(v, res.toString)
        }
      }
    }

    res.map(_.sortBy(d ⇒ (d.varName, d.value)))
  }

  def modelName = "http://test.model/"

  def mkDecl(name: String, owner: Decl, attachments: Attachment*) = {
    val decl = Decl(name, owner)
    decl.addAttachments(attachments: _*)
    decl
  }

  def mkPackage(name: String, owner: Decl) =
    mkDecl(name, owner, Attachment.Package)

  def mkClass(name: String, owner: Decl) =
    mkDecl(name, owner, Attachment.Class)

  def mkDef(name: String, owner: Decl) =
    mkDecl(name, owner, Attachment.Def)

  @Test
  def single_project() = {
    val project = Project("project")

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT * WHERE {
          [a c:Project] c:name ?name .
        }
      """, project) === Seq(Seq(Data("name", "project")))
  }

  @Test
  def multiple_projects() = {
    val project1 = Project("p1")
    val project2 = Project("p2")

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT * WHERE {
          [a c:Project] c:name ?name .
        }
      """, project1, project2) === Seq(
          Seq(Data("name", "p1")),
          Seq(Data("name", "p2")))
  }

  @Test
  def single_artifact() = {
    val project = Project("project")
    val artifact = Artifact(project, "organization", "artifact", "v1")

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT * WHERE {
          [a c:Artifact] c:organization ?organization ; c:name ?name ; c:version ?version .
        }
      """, artifact) === Seq(
          Seq(Data("name", "artifact"), Data("organization", "organization"), Data("version", "v1")))
  }

  @Test
  def multiple_artifacts() = {
    val project = Project("project")
    val artifact1 = Artifact(project, "o1", "a1", "v1")
    val artifact2 = Artifact(project, "o2", "a2", "v2")

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT * WHERE {
          [a c:Artifact] c:organization ?organization ; c:name ?name ; c:version ?version .
        }
      """, artifact1, artifact2) === Seq(
          Seq(Data("name", "a1"), Data("organization", "o1"), Data("version", "v1")),
          Seq(Data("name", "a2"), Data("organization", "o2"), Data("version", "v2")))
  }

  @Test
  def multiple_artifacts_belong_to_same_project() = {
    val project = Project("project")
    val artifact1 = Artifact(project, "o1", "a1", "v1")
    val artifact2 = Artifact(project, "o2", "a2", "v2")

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT DISTINCT * WHERE {
          [a c:Artifact] c:owner [c:name ?name] .
        }
      """, artifact1, artifact2) === Seq(
          Seq(Data("name", "project")))
  }

  @Test
  def files_with_same_name_can_bolong_to_different_artifacts() = {
    val project = Project("project")
    val artifact1 = Artifact(project, "organization", "name", "v1")
    val artifact2 = Artifact(project, "organization", "name", "v2")
    val file1 = File(artifact1, "pkg/Test.scala", Seq(mkClass("Test", mkPackage("pkg", Root))))
    val file2 = File(artifact2, "pkg/Test.scala", Seq(mkClass("Test", mkPackage("pkg", Root))))

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT * WHERE {
          [a c:File] c:owner* [a c:Artifact; c:version ?version]; c:name ?name .
        }
      """, artifact1, artifact2, file1, file2) === Seq(
          Seq(Data("name", "pkg/Test.scala"), Data("version", "v1")),
          Seq(Data("name", "pkg/Test.scala"), Data("version", "v2")))
  }

  @Test
  def the_owner_of_a_project_does_not_exist() = {
    val project = Project("p")

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT * WHERE {
          [a c:Project] c:owner ?owner .
        }
      """, project) === Seq()
  }

  @Test
  def the_owner_of_an_artifact_is_a_project() = {
    val project = Project("p")
    val artifact = Artifact(project, "o", "a", "v1")

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT * WHERE {
          [a c:Artifact] c:owner [c:name ?name] .
        }
      """, artifact) === Seq(Seq(Data("name", "p")))
  }

  @Test
  def the_owner_of_the_top_package_is_an_artifact() = {
    val project = Project("p")
    val artifact = Artifact(project, "o", "a", "v1")
    val file = File(artifact, "pkg/A.scala", Seq(mkPackage("pkg", Root)))

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT * WHERE {
          [a c:Package] c:owner [c:name ?name] .
        }
      """, artifact, file) === Seq(Seq(Data("name", "a")))
  }

  @Test
  def the_owner_of_a_non_top_package_is_a_package() = {
    val project = Project("p")
    val artifact = Artifact(project, "o", "a", "v1")
    val file = File(artifact, "pkg/A.scala", Seq(mkPackage("inner", mkPackage("pkg", Root))))

    ask(modelName, """
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [c:owner [a c:Package]] s:name ?name .
        }
      """, artifact, file) === Seq(Seq(Data("name", "inner")))
  }

  @Test
  def the_owner_of_a_top_level_class_is_a_file() = {
    val project = Project("p")
    val artifact = Artifact(project, "o", "a", "v1")
    val file = File(artifact, "pkg/A.scala", Seq(mkClass("A", mkPackage("pkg", Root))))

    ask(modelName, """
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [a c:Class] c:owner [c:name ?name] .
        }
      """, artifact, file) === Seq(Seq(Data("name", "pkg/A.scala")))
  }

  @Test
  def the_owner_of_a_class_in_the_default_package_is_a_file() = {
    val project = Project("p")
    val artifact = Artifact(project, "o", "n", "v1")
    val file = File(artifact, "A.scala", Seq(mkClass("A", Root)))

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT * WHERE {
          [a c:Class] c:owner [c:name ?name] .
        }
      """, artifact, file) === Seq(Seq(Data("name", "A.scala")))
  }

  @Test
  def the_owner_of_a_file_is_a_package() = {
    val project = Project("p")
    val artifact = Artifact(project, "o", "n", "v1")
    val file = File(artifact, "pkg/A.scala", Seq(mkClass("A", mkPackage("pkg", Root))))

    ask(modelName, """
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [a c:File] c:owner [s:name ?name] .
        }
      """, artifact, file) === Seq(Seq(Data("name", "pkg")))
  }

  @Test
  def multiple_files_can_have_the_same_artifact_as_owner() = {
    val project = Project("p")
    val artifact = Artifact(project, "o", "n", "v1")
    val file1 = File(artifact, "pkg/A.scala", Seq(mkClass("A", mkPackage("pkg", Root))))
    val file2 = File(artifact, "pkg/B.scala", Seq(mkClass("B", mkPackage("pkg", Root))))

    ask(modelName, """
        PREFIX c:<?MODEL?>
        SELECT * WHERE {
          [a c:Class] c:owner* [a c:Artifact; c:name ?name] .
        }
      """, artifact, file1, file2) === Seq(
          Seq(Data("name", "n")),
          Seq(Data("name", "n")))
  }

  @Test
  def the_owner_of_a_def_is_a_class() = {
    val project = Project("p")
    val artifact = Artifact(project, "o", "a", "v1")
    val file = File(artifact, "pkg/A.scala", Seq(mkDef("f", mkClass("A", mkPackage("pkg", Root)))))

    ask(modelName, """
        PREFIX c:<?MODEL?>
        PREFIX s:<http://schema.org/>
        SELECT * WHERE {
          [a c:Def] c:owner [s:name ?name] .
        }
      """, artifact, file) === Seq(Seq(Data("name", "A")))
  }

  @Test
  def xxx(): Unit = {
    val indexer = new Indexer
    val dataset = indexer.mkInMemoryDataset
    println(indexer.writeDataset(dataset) { dataset ⇒
      indexer.withModel(dataset, modelName) { model ⇒
        val schemaName = "Format"
        val rawJson = io.Source.fromFile(s"/home/antoras/dev/scala/amora/schema/$schemaName.schema.jsonld", "UTF-8").mkString
        val gen = new SchemaGenerator
        import spray.json._
        val json = gen.resolveVariables(schemaName, rawJson)

        indexer.addJsonLd(model, json.parseJson)
        val contentVar = "content"
        indexer.withUpdateService(model, gen.mkInsertFormatQuery(schemaName, contentVar)) { pss ⇒
          val generated = gen.mkJsonLdContext(schemaName, json)
          import org.apache.jena.datatypes.BaseDatatype
          pss.setLiteral(contentVar, generated.prettyPrint, new BaseDatatype("http://schema.org/Text"))
        }
        println(indexer.queryResultAsString(modelName, "select * { ?s ?p ?o }", model))
        indexer.doesIdExist(model, gen.mkAmoraSchemaId(schemaName)+"/")
      }
    })
    println(indexer.readDataset(dataset) { dataset ⇒
      val schemaName = "Format"
      val gen = new SchemaGenerator
      indexer.withModel(dataset, modelName) { model ⇒
        indexer.doesIdExist(model, gen.mkAmoraSchemaId(schemaName)+"/")
      }
    })
    dataset.close()
  }
}
