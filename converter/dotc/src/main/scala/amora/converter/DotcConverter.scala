package amora.converter

import scala.collection.mutable.ListBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import amora.converter.protocol.Schema
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.transform.PostTyper

/**
 * Dotc phase that consumes typed trees that are produces by dotc.
 */
final class AmoraPhase extends Phase {
  override def phaseName: String = "amora-phase"

  override def runsAfter = Set(classOf[PostTyper])

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    if (!unit.isJava) {
      val converter = new DotcConverter
      val schema = converter.convert(unit.tpdTree)
      schema foreach println
    }
  }
}

final class DotcConverter(implicit val ctx: Context) {

  private val found = ListBuffer[Schema]()

  /**
   * Extracts the semantic information in `tree` and converts it into a
   * structure described by [[Schema]].
   */
  def convert(tree: tpd.Tree): Try[Seq[Schema]] = {
    found.clear()
    Try(traverse(tree)) match {
      case Success(_) ⇒
        Success(found.toList)
      case Failure(f) ⇒
        Failure(new RuntimeException(s"Conversion of file `${ctx.source.file.absolute}` failed. See underlying issue for more information.", f))
    }
  }

  private def traverse(t: tpd.Tree) = t match {
    case PackageDef(pid, stats) ⇒
  }
}
