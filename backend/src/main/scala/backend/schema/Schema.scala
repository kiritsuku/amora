package backend.schema

trait Schema
final case class Project(name: String) extends Schema

object Schema {

  def mkSparqlUpdate(schemas: Seq[Schema]): String = {
    def mk(o: Schema, sb: StringBuilder, indent: String) = o match {
      case Project(name) ⇒
        sb.append(indent)
        sb.append("<http://amora.center/kb/amora/Project/0.1/")
        sb.append(name)
        sb.append("> a <http://amora.center/kb/amora/Schema/0.1/Project/0.1/> .\n")
        sb.append(indent)
        sb.append("<http://amora.center/kb/amora/Project/0.1/")
        sb.append(name)
        sb.append("> <http://amora.center/kb/amora/Schema/0.1/Project/0.1/name> ")
        sb.append("\"")
        sb.append(name)
        sb.append("\" .\n")
    }

    val sb = new StringBuilder
    sb.append("INSERT DATA {\n")
    schemas foreach {
      mk(_, sb, "  ")
    }
    sb.append("}")
    sb.toString()
  }

}
