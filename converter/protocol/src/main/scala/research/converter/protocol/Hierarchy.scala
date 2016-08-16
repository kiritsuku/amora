package research.converter.protocol

import java.net.URLEncoder

sealed trait Hierarchy {

  private var _attachments = Set[Attachment]()

  final var position: Position = NoPosition

  final def asString: String = this match {
    case Root ⇒
      throw new UnsupportedOperationException("Root can't be represented as a string.")
    case Decl(name, parent) ⇒
      val sig = attachments.collectFirst {
        case Attachment.JvmSignature(signature) ⇒ signature
      }.getOrElse("")
      val paramAtt = attachments.collectFirst {
        case Attachment.Param ⇒ "<param>"
        case Attachment.TypeParam ⇒ "<tparam>"
      }.getOrElse("")
      parent match {
        case Root ⇒
          s"$paramAtt$name$sig"
        case _ ⇒
          s"${parent.asString}.$paramAtt$name$sig"
      }
    case Ref(name, _, _, qualifier) ⇒
      qualifier match {
        case Root ⇒
          s"<ref>$name"
        case _ ⇒
          s"${qualifier.asString}.<ref>$name"
      }
  }

  def attachments: Set[Attachment] = _attachments

  def addAttachments(as: Attachment*): Unit = {
    as foreach (this._attachments += _)
  }

  /**
   * The name of the hierarchy object.
   */
  def name: String

  /**
   * The scope of [[name]], i.e. another hierarchy object in which [[name]] has
   * been defined and where it can be accessed.
   */
  def owner: Hierarchy
}

object Hierarchy {

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

/**
 * Specifies a declaration.
 *
 * [[name]] overrides the super declaration. [[_owner]] is private to this class
 * and exists only because [[owner]] couldn't directly be overridden.
 */
case class Decl(override val name: String, private val _owner: Hierarchy) extends Hierarchy {
  override def owner = _owner
}

/**
 * Specifies a reference to a declaration.
 *
 * [[name]] is the name of the reference. [[refToDecl]] is the declaration to which
 * this reference points to. [[owner]] is the declaration, which contains [[name]].
 * [[qualifier]] specifies how [[name]] refers to [[refToDecl]].
 */
case class Ref(override val name: String, refToDecl: Hierarchy, override val owner: Hierarchy, qualifier: Hierarchy) extends Hierarchy

/**
 * The root or bottom of the hierarchy. The owner refers to itself.
 */
object Root extends Decl("_root_", null) {
  override def toString = "Root"
  override def owner = Root
}
