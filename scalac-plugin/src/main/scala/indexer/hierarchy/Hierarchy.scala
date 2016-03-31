package indexer.hierarchy

sealed trait Hierarchy {

  private var _attachments = Set[Attachment]()

  final var position: Position = NoPosition

  final def asString: String = this match {
    case Decl(name, parent) ⇒
      val sig = attachments.collectFirst {
        case Attachment.JvmSignature(signature) ⇒ signature
      }.getOrElse("")
      s"${parent.asString}.$name$sig"
    case Ref(name, _, _, qualifier) ⇒
      s"${qualifier.asString}.!$name"
    case Root ⇒
      name
  }

  def attachments: Set[Attachment] = _attachments

  def addAttachments(as: Attachment*): Unit = {
    as foreach (this._attachments += _)
  }

  def name: String
}

/**
 * Specifies a declaration.
 *
 * `name` is the name of the declaration, `owner` is the scope of `name`, i.e.
 * another declaration in which `name` has been defined and where it can be
 * accessed.
 */
final case class Decl(override val name: String, owner: Hierarchy) extends Hierarchy

/**
 * Specifies a reference to a declaration.
 *
 * `name` is the name of the reference. `refToDecl` is the declaration to which
 * this reference points to. `owner` is the declaration, which contains `name`.
 * `qualifier` specifies how `name` refers to `refToDecl`.
 */
final case class Ref(override val name: String, refToDecl: Hierarchy, owner: Hierarchy, qualifier: Hierarchy) extends Hierarchy

/**
 * The root or bottom of the hierarchy. It does have a name but not an owner.
 */
final case object Root extends Hierarchy {
  override def name = "_root_"
}
