package indexer.hierarchy

sealed trait Hierarchy {

  private var as = Set[Attachment]()

  final var position: Position = NoPosition

  final def asString: String = this match {
    case Decl(name, Root) ⇒
      name
    case Decl(name, parent) ⇒
      s"${parent.asString}.$name"
    case ThisRef(cls) ⇒
      cls.asString
    case Ref(name, _, _, calledOn) ⇒
      if (calledOn == Root)
        name
      else
        s"${calledOn.asString}.$name"
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

final case class Decl(override val name: String, owner: Declaration) extends Declaration

sealed trait Reference extends Hierarchy

final case class Ref(override val name: String, refToDecl: Declaration, owner: Declaration, calledOn: Declaration) extends Reference

final case class ThisRef(cls: Decl) extends Reference {
  override def name = throw new UnsupportedOperationException(s"`$this` does not have a name.")
}

final case object Root extends Reference with Declaration {
  override def name = throw new UnsupportedOperationException(s"`$this` does not have a name.")
}
