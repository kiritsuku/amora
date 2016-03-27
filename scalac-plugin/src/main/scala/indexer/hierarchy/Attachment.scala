package indexer.hierarchy

trait Attachment {
  def asString: String
}
object Attachment {
  case object Package extends Attachment { override def asString = "package" }
  case object Class extends Attachment { override def asString = "class" }
  case object Trait extends Attachment { override def asString = "trait" }
  case object Object extends Attachment { override def asString = "object" }
  case object Abstract extends Attachment { override def asString = "abstract" }
  case object Def extends Attachment { override def asString = "def" }
  case object Val extends Attachment { override def asString = "val" }
  case object Var extends Attachment { override def asString = "var" }
  case object Lazy extends Attachment { override def asString = "lazy" }
  case object TypeParam extends Attachment { override def asString = "tparameter" }
  case object Param extends Attachment { override def asString = "parameter" }
  case object TermRef extends Attachment { override def asString = "termref" }
  case object TypeRef extends Attachment { override def asString = "typeref" }
  case object Ref extends Attachment { override def asString = "reference" }
}
