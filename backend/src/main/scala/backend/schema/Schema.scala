package backend.schema

import java.net.URLEncoder

import research.converter.protocol._

trait Schema
final case class Project(name: String) extends Schema
final case class Artifact(owner: Project, organization: String, name: String, version: String) extends Schema
final case class File(owner: Schema, name: String) extends Schema
final case class Package(name: String, owner: Schema) extends Schema
final case class Class(name: String, owner: Schema) extends Schema
final case class Def(name: String, owner: Schema) extends Schema

object Schema {

  private def mkShortId(s: Schema): String = s match {
    case Project(name) ⇒
      name
    case Artifact(owner, organization, name, version) ⇒
      s"${mkShortId(owner)}/$organization/$name/$version"
    case File(owner, name) ⇒
      s"${mkShortId(owner)}/$name"
    case Package(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
    case Class(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
    case Def(name, owner) ⇒
      s"${mkShortId(owner)}/$name"
  }

  def mkId(s: Schema): String = s match {
    case _: Project ⇒
      s"http://amora.center/kb/amora/Project/0.1/${mkShortId(s)}"
    case _: Artifact ⇒
      s"http://amora.center/kb/amora/Artifact/0.1/${mkShortId(s)}"
    case _: File ⇒
      s"http://amora.center/kb/amora/File/0.1/${mkShortId(s)}"
    case _: Package ⇒
      s"http://amora.center/kb/amora/Package/0.1/${mkShortId(s)}"
    case _: Class ⇒
      s"http://amora.center/kb/amora/Class/0.1/${mkShortId(s)}"
    case _: Def ⇒
      s"http://amora.center/kb/amora/Def/0.1/${mkShortId(s)}"
  }

  def mkDefn(s: Schema): String = s match {
    case _: Project ⇒
      s"http://amora.center/kb/amora/Schema/0.1/Project/0.1"
    case _: Artifact ⇒
      s"http://amora.center/kb/amora/Schema/0.1/Artifact/0.1"
    case _: File ⇒
      s"http://amora.center/kb/amora/Schema/0.1/File/0.1"
    case _: Package ⇒
      s"http://amora.center/kb/amora/Schema/0.1/Package/0.1"
    case _: Class ⇒
      s"http://amora.center/kb/amora/Schema/0.1/Class/0.1"
    case _: Def ⇒
      s"http://amora.center/kb/amora/Schema/0.1/Def/0.1"
  }

  def mkSparqlUpdate(schemas: Seq[Schema]): String = {
    val sb = new StringBuilder

    def mk(s: Schema): String = s match {
      case Project(name) ⇒
        val id = mkId(s)
        val defn = mkDefn(s)
        val tpe = "http://schema.org/Text"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/name> "$name"^^<$tpe> .
        |""".stripMargin)
        id
      case Artifact(owner, organization, name, version) ⇒
        val oid = mk(owner)
        val id = mkId(s)
        val defn = mkDefn(s)
        val tpe = "http://schema.org/Text"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/organization> "$organization"^^<$tpe> .
                      |  <$id> <$defn/name> "$name"^^<$tpe> .
                      |  <$id> <$defn/version> "$version"^^<$tpe> .
        |""".stripMargin)
        id
      case File(owner, fname) ⇒
        val oid = mk(owner)
        val id = mkId(s)
        val defn = mkDefn(s)
        val tpe = "http://schema.org/Text"
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$fname"^^<$tpe> .
        |""".stripMargin)
        id
      case Package(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$name"^^<http://schema.org/Text> .
        |""".stripMargin)
        id
      case Class(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$name"^^<http://schema.org/Text> .
        |""".stripMargin)
        id
      case Def(name, parent) ⇒
        val oid = mk(parent)
        val id = mkId(s)
        val defn = mkDefn(s)
        sb.append(s"""|  <$id> a <$defn/> .
                      |  <$id> <$defn/owner> <$oid> .
                      |  <$id> <$defn/name> "$name"^^<http://schema.org/Text> .
        |""".stripMargin)
        id
    }

    sb.append("INSERT DATA {\n")
    schemas foreach mk
    sb.append("}")
    sb.toString()
  }

}

object HierarchySchema {

  def mkSparqlUpdate(prefix: String, origin: String, data: Seq[Hierarchy]): String = {
    val sb = new StringBuilder

    def loop(h: Hierarchy): Unit = h match {
      case Root ⇒
      case decl @ Decl(name, owner) ⇒
        val n = encode(name)
        val path = prefix + "/" + mkPath(decl)
        val tpe = decl.attachments.collectFirst {
          case Attachment.Class ⇒ "Class"
          case Attachment.Package ⇒ "Package"
          case Attachment.Def ⇒ "Def"
        }.getOrElse("Decl")

        val tpeString = s"http://amora.center/kb/amora/Schema/0.1/$tpe/0.1"
        sb.append(s"  <$path> a <$tpeString/> .\n")
        sb.append(s"""  <$path> <$tpeString/name> "$n" .""" + "\n")

        if (h.attachments.nonEmpty) {
          val elems = h.attachments.map("\"" + _.asString + "\"").mkString(", ")
          sb.append(s"  <$path> <$tpeString/attachment> $elems .\n")
        }

        owner match {
          case Root ⇒
            sb.append(s"  <$path> <$tpeString/owner> <$origin> .\n")
          case _: Decl ⇒
            val o = encode(owner.asString).replace('.', '/')
            sb.append(s"  <$path> <$tpeString/owner> <$prefix/$o> .\n")
          case _: Ref ⇒
        }
      case Ref(name, refToDecl, owner, qualifier) ⇒
    }

    sb.append("INSERT DATA {\n")
    data foreach loop
    sb.append("}")
    sb.toString
  }

  private def uniqueRef(pos: Position) = pos match {
    case RangePosition(start, _) ⇒
      s"/$start"
    case _ ⇒
      ""
  }

  def encode(str: String): String =
    URLEncoder.encode(str, "UTF-8")

  private def mkPath(decl: Decl) = {
    val Decl(name, owner) = decl
    val n = encode(name)
    val ownerPath = owner match {
      case Root ⇒
        ""
      case _: Decl ⇒
        encode(owner.asString).replace('.', '/')
      case _: Ref ⇒
        val path = encode(owner.asString).replace('.', '/')
        val h = uniqueRef(owner.position)
        s"$path/$h"
    }
    val sig = decl.attachments.collectFirst {
      case Attachment.JvmSignature(signature) ⇒ encode(signature)
    }.getOrElse("")
    val paramAtt = encode(decl.attachments.collectFirst {
      case Attachment.Param ⇒ "<param>"
      case Attachment.TypeParam ⇒ "<tparam>"
    }.getOrElse(""))
    val originPath = if (ownerPath.isEmpty) "" else ownerPath + "/"
    val fullPath = s"$originPath$paramAtt$n$sig"
    fullPath
  }
}
