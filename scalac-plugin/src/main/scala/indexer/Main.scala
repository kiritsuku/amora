package indexer

import java.net.URL

import org.apache.jena.query.ResultSetFormatter

import indexer.hierarchy._
import util.LoggerConfig

object Main extends App with LoggerConfig {

  import Indexer._

  val m = Member(Class(Package(Seq("a", "b", "c")), "SomeClass"), "method")
  val c = Class(Package(Seq("d", "e", "f")), "SomeType")
  val data = Seq(
      Class(Package(Seq("a", "b", "c")), "TestClass"),
      Class(Package(Seq("a", "b", "c")), "SomeClass"),
      Class(Package(Seq("a", "b", "c")), "AnotherClass"),
      c,
      Class(Package(Seq("d", "e", "f")), "SomeClass"),
      m,
      Member(Class(Package(Seq("d", "e", "f")), "SomeClass"), "toList"),
      Member(Class(Package(Seq("d", "e", "f")), "SomeType"), "someFunc"),
      Member(Class(Package(Seq("a", "b", "c")), "AnotherClass"), "anotherMethod"),
      TypeRef(m, c)
  )
  addData("testfile.scala", data)
//  httpRequest()

  /**
   * The location where we want to store our data. Since we can run the program
   * from different locations we need a stable location which never changes.
   * This method returns such a path.
   */
  def storageLocation = {
    val p = getClass.getProtectionDomain.getCodeSource.getLocation.getPath
    val projectName = "scalac-plugin/"
    val i = p.indexOf(projectName)
    p.substring(0, i+projectName.length)
  }

  private def addData(filename: String, data: Seq[Hierarchy]) = {
    val modelName = "http://test.model/"

    withDataset(s"$storageLocation/dataset") { dataset ⇒
      withModel(dataset, modelName)(add(modelName, filename, data))
      withModel(dataset, modelName) { model ⇒
        val q = selectAll(modelName)
        queryResultAsString(modelName, q, model) foreach println
      }
    }
  }

  private def findClass(modelName: String) = s"""
    PREFIX c:<$modelName>
    PREFIX s:<http://schema.org/>
    SELECT ?elem ?p ?o WHERE {
      ?elem s:name ?name .
      FILTER (str(?name) = "SomeClass") .
      ?elem c:tpe "class" .
      ?elem c:declaration ?decl .
      FILTER regex(str(?decl), "_root_/.*/c") .
      ?elem ?p ?o .
    }
  """

  private def selectAll(modelName: String) = s"""
    SELECT * WHERE {
      ?s ?p ?o .
    }
  """

  private def httpRequest() = {
    val endpoint = new URL("http://dbpedia.org/sparql")
    val query = """
      PREFIX dbo: <http://dbpedia.org/ontology/>
      PREFIX dbp: <http://dbpedia.org/property/>
      SELECT DISTINCT ?language ?p ?o WHERE {
       ?language a dbo:ProgrammingLanguage .
       ?language dbp:name ?name .
       FILTER(STR(?name) = "Scala") .
       ?language ?p ?o .
      } LIMIT 100
    """

    withSparqlService(endpoint.toString(), query) map { r ⇒
      ResultSetFormatter.out(System.out, r)
    }
  }
}
