package backend.schema

import research.converter.protocol._

trait Schema
final case class Project(name: String) extends Schema
final case class Artifact(owner: Project, organization: String, name: String, version: String) extends Schema
final case class File(owner: Schema, name: String, data: Seq[Hierarchy]) extends Schema

object Schema {

  def mkSparqlUpdate(schemas: Seq[Schema]): String = {
    val sb = new StringBuilder

    def mkId(o: Schema) = o match {
      case Project(name) ⇒
        s"http://amora.center/kb/amora/Project/0.1/$name"
      case Artifact(owner, organization, name, version) ⇒
        s"http://amora.center/kb/amora/Artifact/0.1/$organization/$name/$version"
      case File(Artifact(_, organization, name, version), fname, _) ⇒
        s"http://amora.center/kb/amora/File/0.1/$organization/$name/$version/$fname"
    }

    def mk(o: Schema, indent: String): String = o match {
      case Project(name) ⇒
        val id = mkId(o)
        val defn = "http://amora.center/kb/amora/Schema/0.1/Project/0.1"
        val tpe = "http://schema.org/Text"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/name> "$name"^^<$tpe> .
        |""".stripMargin)
        id
      case Artifact(owner, organization, name, version) ⇒
        val oid = mk(owner, indent)
        val id = mkId(o)
        val defn = "http://amora.center/kb/amora/Schema/0.1/Artifact/0.1"
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
        val defn = "http://amora.center/kb/amora/Schema/0.1/File/0.1"
        val tpe = "http://schema.org/Text"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$fname"^^<$tpe> .
        |""".stripMargin)
        data foreach {
          mkHierarchy(owner, _)
        }
        id
    }

    def mkHierarchy(owner: Artifact, h: Hierarchy) = h match {
      case decl @ Decl(name, parent) ⇒
        val id = s"http://amora.center/kb/amora/Package/0.1/${owner.organization}/${owner.name}/${owner.version}/$name"
        val tpe = decl.attachments.collectFirst {
          case Attachment.Package ⇒ "Package"
        }.getOrElse("Decl")
        val defn = s"http://amora.center/kb/amora/Schema/0.1/$tpe/0.1"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <${mkId(owner)}> .
                      |  <$id> <$defn/name> "$name"^^<http://schema.org/Text> .
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
