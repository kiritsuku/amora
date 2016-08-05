package backend.schema

trait Schema
final case class Project(name: String) extends Schema
final case class Artifact(project: Project, organization: String, name: String, version: String) extends Schema

object Schema {

  def mkSparqlUpdate(schemas: Seq[Schema]): String = {
    val sb = new StringBuilder

    def mk(o: Schema, indent: String): Unit = o match {
      case Project(name) ⇒
        val id = s"http://amora.center/kb/amora/Project/0.1/$name"
        val defn = "http://amora.center/kb/amora/Schema/0.1/Project/0.1"
        val tpe = "http://schema.org/Text"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/name> "$name"^^<$tpe> .
        |""".stripMargin)
      case Artifact(project, organization, name, version) ⇒
        mk(project, indent)
        val id = s"http://amora.center/kb/amora/Artifact/0.1/$organization/$name/$version"
        val defn = "http://amora.center/kb/amora/Schema/0.1/Artifact/0.1"
        val tpe = "http://schema.org/Text"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/organization> "$organization"^^<$tpe> .
                      |  <$id> <$defn/name> "$name"^^<$tpe> .
                      |  <$id> <$defn/version> "$version"^^<$tpe> .
        |""".stripMargin)
    }

    sb.append("INSERT DATA {\n")
    schemas foreach {
      mk(_, "  ")
    }
    sb.append("}")
    sb.toString()
  }

}
