package indexer.hierarchy

sealed trait Hierarchy {

  private var _attachments = Set[Attachment]()

  final var position: Position = NoPosition

  final def asString: String = this match {
    case Root ⇒
      name
    case Decl(name, parent) ⇒
      val sig = attachments.collectFirst {
        case Attachment.JvmSignature(signature) ⇒ signature
      }.getOrElse("")
      val paramAtt = attachments.collectFirst {
        case Attachment.Param ⇒ "<param>"
        case Attachment.TypeParam ⇒ "<tparam>"
      }.getOrElse("")
      s"${parent.asString}.$paramAtt$name$sig"
    case Ref(name, _, _, qualifier) ⇒
      s"${qualifier.asString}.<ref>$name"
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
