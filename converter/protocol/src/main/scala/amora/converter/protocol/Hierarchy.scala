package amora.converter.protocol

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
      val att = attachments.collectFirst {
        case Attachment.Param ⇒ "<param>"
        case Attachment.TypeParam ⇒ "<tparam>"
        case Attachment.Object ⇒ "<object>"
      }.getOrElse("")
      parent match {
        case Root ⇒
          s"$att$name$sig"
        case _ ⇒
          s"${parent.asString}.$att$name$sig"
      }
    case Ref(name, refToDecl, _, _) ⇒
      val sig = refToDecl match {
        case d: Decl ⇒
          d.attachments.collectFirst {
            case Attachment.JvmSignature(signature) ⇒ signature
          }.getOrElse("")
        case _ ⇒
          ""
      }
      refToDecl.owner match {
        case Root ⇒
          if (name == "this")
            s"<ref>${refToDecl.asInstanceOf[Decl].name}$sig"
          else
            s"<ref>$name$sig"
        case o ⇒
          if (name == "this")
            s"${o.asString}.<ref>${refToDecl.asInstanceOf[Decl].name}$sig"
          else
            s"${o.asString}.<ref>$name$sig"
      }
    case s @ Scope(parent) ⇒
      s"${parent.asString}.${s.attachmentAsString}"
  }

  def attachments: Set[Attachment] = _attachments

  def addAttachments(as: Attachment*): Unit = {
    as foreach (this._attachments += _)
  }

  /**
   * The scope of [[name]], i.e. another hierarchy object in which [[name]] has
   * been defined and where it can be accessed.
   */
  def owner: Hierarchy
}

case class Scope(override val owner: Hierarchy) extends Hierarchy {
  def attachmentAsString = {
    attachments.collectFirst {
      case Attachment.If ⇒ "<if>"
      case Attachment.Else ⇒ "<else>"
      case Attachment.Try ⇒ "<try>"
      case Attachment.Finally ⇒ "<finally>"
      case Attachment.Case ⇒ "<case>"
      case Attachment.Catch ⇒ "<catch>"
      case Attachment.Match ⇒ "<match>"
      case Attachment.While ⇒ "<while>"
      case Attachment.Do ⇒ "<do>"
      case Attachment.Implicit ⇒ "<implicit>"
    }.getOrElse(throw new IllegalStateException(s"Scope `$this` has no attachment."))
  }
}

sealed trait HierarchyWithName extends Hierarchy {

  /**
   * The name of the hierarchy object.
   */
  def name: String
}

/**
 * Specifies a declaration.
 *
 * [[name]] overrides the super declaration. [[_owner]] is private to this class
 * and exists only because [[owner]] couldn't directly be overridden.
 */
case class Decl(override val name: String, private val _owner: Hierarchy) extends HierarchyWithName {
  override def owner = _owner
}

/**
 * Specifies a reference to a declaration.
 *
 * [[name]] is the name of the reference. [[refToDecl]] is the declaration to which
 * this reference points to. [[owner]] is the declaration, which contains [[name]].
 * [[calledOn]] specifies how [[name]] refers to [[refToDecl]].
 */
case class Ref(override val name: String, refToDecl: Hierarchy, override val owner: Hierarchy, calledOn: Option[Hierarchy]) extends HierarchyWithName

/**
 * The root or bottom of the hierarchy. The owner refers to itself.
 */
object Root extends Decl("_root_", null) {
  override def toString = "Root"
  override def owner = Root
}
