package amora.backend
package indexer

import scala.util._

import org.junit.Test

import amora.backend.schema._
import amora.converter.protocol._
import amora.api._

class JavaBytecodeIndexerTest {

  import amora.TestUtils._

  val debugTests = true

  case class Data(varName: String, value: String)

  def ask(modelName: String, rawQuery: String, data: (String, String)*): Seq[Data] = {
    val indexer = new Indexer(modelName)
    val dataset = indexer.mkInMemoryDataset
    val query = rawQuery.replaceFirst("""\?MODEL\?""", modelName)
    val res = indexer.writeDataset(dataset) { dataset ⇒
      indexer.withModel(dataset) { model ⇒
        val jindexer = new JavaBytecodeIndexer(IgnoreLogger)
        val origin = Artifact(Project("testProject"), "o", "n", "v1")
        jindexer.bytecodeToHierarchy(data) match {
          case Success(data) ⇒
            data foreach {
              case (fileName, data) ⇒

                def asSchemaPackage(decl: Hierarchy): Schema = decl match {
                  case Root ⇒ origin
                  case Decl(name, owner) ⇒ Package(name, asSchemaPackage(owner))
                  case _ ⇒ ???
                }
                val pkg = data.collectFirst {
                  case d if d.attachments(Attachment.Package) ⇒ d
                }
                val s = pkg.map(asSchemaPackage).map(pkg ⇒ File(pkg, fileName)).getOrElse(File(origin, fileName))
                indexer.withUpdateService(model, Schema.mkSparqlUpdate(Seq(s)))(_ ⇒ ())
                indexer.withUpdateService(model, Schema.mkSparqlUpdate(s, data))(_ ⇒ ())
            }
          case Failure(f) ⇒
            throw f
        }

        if (debugTests) {
          println(sparqlQuery"select * { ?s ?p ?o }".runOnModel(model).asStringTable)
          println(query.asSparqlQuery.runOnModel(model).asStringTable)
        }
        indexer.flattenedQueryResult(query, model) { (v, q) ⇒
          val res = q.get(v)
          require(res != null, s"The variable `$v` does not exist in the result set.")
          Data(v, res.toString)
        }
      }
    }

    res.sortBy(d ⇒ (d.varName, d.value))
  }

  def modelName = "http://test.model/"

  @Test
  def classes() = {
    ask(modelName, s"""
        prefix c:<http://amora.center/kb/amora/Schema/Class/>
        select * where {
          [a c:] c:name ?name .
        }
      """,
      "X.java" → """
        public class X {}
      """) === Seq(Data("name", "X"))
  }

  @Test
  def methods() = {
    ask(modelName, s"""
        prefix c:<http://amora.center/kb/amora/Schema/Def/>
        select * where {
          [a c:] c:name ?name .
        }
      """,
      "X.java" → """
        public class X {
          int i() {
            return 0;
          }
          int j(int i) {
            return 0;
          }
        }
      """) === Seq(Data("name", "i"), Data("name", "j"))
  }

  @Test
  def fields() = {
    ask(modelName, s"""
        prefix c:<http://amora.center/kb/amora/Schema/Var/>
        select * where {
          [a c:] c:name ?name .
        }
      """,
      "X.java" → """
        public class X {
          int i = 0;
          int j = 0;
        }
      """) === Seq(Data("name", "i"), Data("name", "j"))
  }

  @Test
  def index_method_parameter_as_vars() = {
    ask(modelName, s"""
        prefix c:<http://amora.center/kb/amora/Schema/Var/>
        select * where {
          [a c:] c:name ?name .
        }
      """,
      "X.java" → """
        public class X {
          void f(int i, int j) {}
        }
      """) === Seq(Data("name", "i"), Data("name", "j"))
  }

  @Test
  def index_method_parameter_as_params() = {
    ask(modelName, s"""
        prefix param:<http://amora.center/kb/amora/Flag/param>
        prefix c:<http://amora.center/kb/amora/Schema/Var/>
        select * where {
          [c:flag param:] c:name ?name .
        }
      """,
      "X.java" → """
        public class X {
          void f(int i, int j) {}
        }
      """) === Seq(Data("name", "i"), Data("name", "j"))
  }
}
