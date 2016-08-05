package backend.schema

trait Schema
final case class Project(name: String) extends Schema

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
    }

    sb.append("INSERT DATA {\n")
    schemas foreach {
      mk(_, "  ")
    }
    sb.append("}")
    sb.toString()
  }

}
