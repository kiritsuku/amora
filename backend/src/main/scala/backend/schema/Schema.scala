package backend.schema

trait Schema
final case class Project(name: String) extends Schema
final case class Artifact(owner: Project, organization: String, name: String, version: String) extends Schema
final case class File(owner: Schema, name: String, data: Seq[Schema]) extends Schema
final case class Package(name: String, owner: Schema) extends Schema

object Schema {

  def mkSparqlUpdate(schemas: Seq[Schema]): String = {
    val sb = new StringBuilder

    def mkShortId(o: Schema): String = o match {
      case Project(name) ⇒
        name
      case Artifact(_, organization, name, version) ⇒
        s"$organization/$name/$version"
      case File(owner, name, _) ⇒
        s"${mkShortId(owner)}/$name"
      case Package(name, owner) ⇒
        s"${mkShortId(owner)}/$name"
    }

    def mkId(o: Schema) = o match {
      case _: Project ⇒
        s"http://amora.center/kb/amora/Project/0.1/${mkShortId(o)}"
      case _: Artifact ⇒
        s"http://amora.center/kb/amora/Artifact/0.1/${mkShortId(o)}"
      case _: File ⇒
        s"http://amora.center/kb/amora/File/0.1/${mkShortId(o)}"
      case _: Package ⇒
        s"http://amora.center/kb/amora/Package/0.1/${mkShortId(o)}"
    }

    def mkDefn(o: Schema) = o match {
      case _: Project ⇒
        s"http://amora.center/kb/amora/Schema/0.1/Project/0.1"
      case _: Artifact ⇒
        s"http://amora.center/kb/amora/Schema/0.1/Artifact/0.1"
      case _: File ⇒
        s"http://amora.center/kb/amora/Schema/0.1/File/0.1"
      case _: Package ⇒
        s"http://amora.center/kb/amora/Schema/0.1/Package/0.1"
    }

    def mk(o: Schema, indent: String): String = o match {
      case Project(name) ⇒
        val id = mkId(o)
        val defn = mkDefn(o)
        val tpe = "http://schema.org/Text"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/name> "$name"^^<$tpe> .
        |""".stripMargin)
        id
      case Artifact(owner, organization, name, version) ⇒
        val oid = mk(owner, indent)
        val id = mkId(o)
        val defn = mkDefn(o)
        val tpe = "http://schema.org/Text"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/organization> "$organization"^^<$tpe> .
                      |  <$id> <$defn/name> "$name"^^<$tpe> .
                      |  <$id> <$defn/version> "$version"^^<$tpe> .
        |""".stripMargin)
        id
      case File(owner @ Artifact(_, organization, name, version), fname, data) ⇒
        val oid = mk(owner, indent)
        val id = mkId(o)
        val defn = mkDefn(o)
        val tpe = "http://schema.org/Text"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$fname"^^<$tpe> .
        |""".stripMargin)
        data foreach {
          mk(_, indent)
        }
        id
      case Package(name, parent) ⇒
        val oid = mk(parent, indent)
        val id = mkId(o)
        val defn = s"http://amora.center/kb/amora/Schema/0.1/Package/0.1"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$name"^^<http://schema.org/Text> .
        |""".stripMargin)
        id
    }

    sb.append("INSERT DATA {\n")
    schemas foreach {
      mk(_, "  ")
    }
    sb.append("}")
    sb.toString()
  }

}
