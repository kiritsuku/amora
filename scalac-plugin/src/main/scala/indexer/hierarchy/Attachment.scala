package indexer.hierarchy

trait Attachment {
  def asString: String
}

final case object PackageDecl extends Attachment { override def asString = "package" }
final case object ClassDecl extends Attachment { override def asString = "class" }
final case object TraitDecl extends Attachment { override def asString = "trait" }
final case object ObjectDecl extends Attachment { override def asString = "object" }
final case object AbstractClassDecl extends Attachment { override def asString = "abstract-class" }
final case object DefDecl extends Attachment { override def asString = "def" }
final case object ValDecl extends Attachment { override def asString = "val" }
final case object VarDecl extends Attachment { override def asString = "var" }
final case object LazyValDecl extends Attachment { override def asString = "lazy-val" }
