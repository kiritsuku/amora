package indexer.hierarchy

sealed trait Hierarchy {
  override def toString = this match {
    case Package(pkgs) ⇒
      pkgs mkString "."
    case Class(pkg: Package, name) ⇒
      if (pkg.pkgs.isEmpty)
        name
      else
        s"$pkg.$name"
    case Class(decl, name) ⇒
      s"$decl.$name"
    case Member(parent, name) ⇒
      s"$parent.$name"
    case TermRef(name, outer) ⇒
      if (outer == Root)
        name
      else
        s"$outer.$name"
    case TypeRef(name, _) ⇒
      s"$name"
    case ThisRef(cls) ⇒
      s"$cls.this"
    case Root ⇒
      "_root_"
  }
}

sealed trait Declaration extends Hierarchy

final case class Package(pkgs: Seq[String]) extends Declaration

final case class Class(decl: Declaration, name: String) extends Declaration

final case class Member(parent: Declaration, name: String) extends Declaration

sealed trait Reference extends Hierarchy

final case class TermRef(name: String, outer: Reference) extends Reference

final case class TypeRef(name: String, decl: Declaration) extends Reference

final case class ThisRef(cls: Class) extends Reference

final case object Root extends Reference
