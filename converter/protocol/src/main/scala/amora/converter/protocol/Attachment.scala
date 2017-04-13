package amora.converter.protocol

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
  case object TypeParam extends Attachment { override def asString = "tparam" }
  case object Param extends Attachment { override def asString = "param" }
  case object Ref extends Attachment { override def asString = "ref" }
  case object Function extends Attachment { override def asString = "function" }
  case object If extends Attachment { override def asString = "if" }
  case object Else extends Attachment { override def asString = "else" }
  case object Try extends Attachment { override def asString = "try" }
  case object Catch extends Attachment { override def asString = "catch" }
  case object Finally extends Attachment { override def asString = "finally" }
  case object Match extends Attachment { override def asString = "match" }
  case object Case extends Attachment { override def asString = "case" }
  case object While extends Attachment { override def asString = "while" }
  case object Do extends Attachment { override def asString = "do" }
  case object Constructor extends Attachment { override def asString = "constructor" }
  case object Implicit extends Attachment { override def asString = "implicit" }
  case object Repeated extends Attachment { override def asString = "repeated" }

  case class JvmSignature(signature: String) extends Attachment { override def asString = signature }
  case class JvmClass(jvmClass: String) extends Attachment { override def asString = jvmClass }
  case class SourceFile(source: Schema) extends Attachment { override def asString = source.toString }
  /**
   * The order of expression applications. In `List.apply(1).drop(1).take(1).sum`
   * the order would be `List → apply → drop → take → sum`.
   */
  case class Order(nr: Int) extends Attachment { override def asString = nr.toString }
}
