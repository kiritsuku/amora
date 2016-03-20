package indexer.hierarchy

sealed trait Hierarchy {

  private var as = Set[Attachment]()

  final def asString: String = this match {
    case Decl(name, Root) ⇒
      name
    case Decl(name, parent) ⇒
      s"${parent.asString}.$name"
    case Member(parent, name) ⇒
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

  def position: Position = NoPosition

  def attachments: Set[Attachment] = as

  def addAttachments(as: Attachment*): Unit = {
    as foreach (this.as += _)
  }
}

sealed trait Declaration extends Hierarchy

final case class Decl(name: String, parent: Declaration) extends Declaration

final case class Member(parent: Declaration, name: String) extends Declaration

sealed trait Reference extends Hierarchy

final case class TermRef(name: String, outer: Reference) extends Reference

/** `usage` is the location where the type is used, `decl` is the type that is used. */
final case class TypeRef(usage: Hierarchy, decl: Declaration) extends Reference

final case class ThisRef(cls: Decl) extends Reference

final case object Root extends Reference with Declaration
