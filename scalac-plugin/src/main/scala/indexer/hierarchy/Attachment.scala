package indexer.hierarchy

trait Attachment {
  def asString: String
}
object Attachment {
  case object PackageDecl extends Attachment { override def asString = "package" }
  case object ClassDecl extends Attachment { override def asString = "class" }
  case object TraitDecl extends Attachment { override def asString = "trait" }
  case object ObjectDecl extends Attachment { override def asString = "object" }
  case object AbstractDecl extends Attachment { override def asString = "abstract" }
  case object DefDecl extends Attachment { override def asString = "def" }
  case object ValDecl extends Attachment { override def asString = "val" }
  case object VarDecl extends Attachment { override def asString = "var" }
  case object LazyDecl extends Attachment { override def asString = "lazy" }
  case object TypeParamDecl extends Attachment { override def asString = "tparameter" }
  case object ParamDecl extends Attachment { override def asString = "parameter" }
  case object TermRef extends Attachment { override def asString = "termref" }
  case object TypeRef extends Attachment { override def asString = "typeref" }
}
