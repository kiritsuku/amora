package backend.indexer

import org.junit.Test

import backend.actors.IndexerMessage._
import research.converter.protocol._

class ModelIndexerTest {
  import backend.TestUtils._

  case class Data(varName: String, value: String)

  def ask(modelName: String, rawQuery: String, data: Indexable*): Seq[Seq[Data]] = {
    val indexer = new Indexer(modelName)
    val dataset = indexer.mkInMemoryDataset
    val query = rawQuery.replaceFirst("""\?MODEL\?""", modelName)
    val res = indexer.writeDataset(dataset) { dataset ⇒
      indexer.withModel(dataset) { model ⇒
        data foreach (indexer.add(model, _))

        if (debugTests) {
          println(indexer.queryResultAsString("select * { ?s ?p ?o }", model))
          println(indexer.queryResultAsString(query, model))
        }

        indexer.queryResult(query, model) { (v, q) ⇒
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
}
