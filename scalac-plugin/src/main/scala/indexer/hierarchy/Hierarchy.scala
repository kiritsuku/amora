package indexer.hierarchy

sealed trait Hierarchy {

  private var as = Set[Attachment]()

  final var position: Position = NoPosition

  final def asString: String = this match {
    case Decl(name, Root) ⇒
      name
    case Decl(name, parent) ⇒
      s"${parent.asString}.$name"
    case TermRef(name, outer) ⇒
      if (outer == Root)
        name
      else
        s"${outer.asString}.$name"
    case TypeRef(_, decl) ⇒
      decl.asString
    case ThisRef(cls) ⇒
      cls.asString
    case Root ⇒
      "_root_"
  }

  def attachments: Set[Attachment] = as

  def addAttachments(as: Attachment*): Unit = {
    as foreach (this.as += _)
  }

  def name: String
}

sealed trait Declaration extends Hierarchy

final case class Decl(override val name: String, parent: Declaration) extends Declaration

sealed trait Reference extends Hierarchy

final case class TermRef(override val name: String, outer: Reference) extends Reference

/** `usage` is the location where the type is used, `decl` is the type that is used. */
final case class TypeRef(usage: Hierarchy, decl: Declaration) extends Reference {
  override def name = decl match {
    case Decl(name, _) ⇒ name
    case d ⇒ throw new UnsupportedOperationException(s"`$d` does not have a name.")
  }
}

final case class ThisRef(cls: Decl) extends Reference {
  override def name = throw new UnsupportedOperationException(s"`$this` does not have a name.")
}

final case object Root extends Reference with Declaration {
  override def name = throw new UnsupportedOperationException(s"`$this` does not have a name.")
}
