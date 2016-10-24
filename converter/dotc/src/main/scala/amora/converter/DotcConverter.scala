package amora.converter

import scala.collection.mutable.ListBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import amora.converter.protocol.Hierarchy
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.transform.PostTyper
import dotty.tools.dotc.util.Positions.Position

/**
 * Dotc phase that consumes typed trees that are produces by dotc.
 */
final class AmoraPhase(report: (CompilationUnit, Try[Seq[Hierarchy]]) ⇒ Unit) extends Phase {
  override def phaseName: String = "amora-phase"

  override def runsAfter = Set(classOf[PostTyper])

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    val converter = new DotcConverter
    report(unit, converter.convert(unit.tpdTree))
  }
}

final class DotcConverter(implicit val ctx: Context) {
  import amora.converter.{ protocol ⇒ h }
  import amora.converter.protocol.{ Attachment ⇒ a }

  private val found = ListBuffer[Hierarchy]()

  /**
   * Extracts the semantic information in `tree` and converts it into a
   * structure described by [[Schema]].
   */
  def convert(tree: tpd.Tree): Try[Seq[Hierarchy]] = {
    found.clear()
    Try(traverse(tree)) match {
      case Success(_) ⇒
        Success(found.toList)
      case Failure(f) ⇒
        Failure(new RuntimeException(s"Conversion of file `${ctx.source.file.absolute}` failed. See underlying issue for more information.", f))
    }
  }
  private def mkDecl(sym: Symbol, owner: h.Hierarchy): h.Decl = {
    val decl = h.Decl(sym.name.toString, owner)
    decl
  }

  private def typeDef(owner: h.Hierarchy, t: tpd.TypeDef): Unit = {
    val decl = mkDecl(t.symbol, owner)
    setPosition(decl, t.pos)
    found += decl
  }

  private def body(owner: h.Hierarchy, t: tpd.Tree): Unit = t match {
    case t: TypeDef[_] ⇒
      typeDef(owner, t)
  }

  private def packageDef(t: tpd.Tree): h.Decl = t match {
    case Select(qualifier, name) ⇒
      val decl = mkDecl(t.symbol, packageDef(qualifier))
      decl.addAttachments(a.Package)
      setPosition(decl, t.pos)
      decl
    case Ident(Names.EMPTY_PACKAGE) ⇒
      h.Root
    case Ident(_) ⇒
      val decl = mkDecl(t.symbol, h.Root)
      decl.addAttachments(a.Package)
      t.pos
      setPosition(decl, t.pos)
      decl
  }

  private def traverse(t: tpd.Tree) = t match {
    case PackageDef(pid, stats) ⇒
      val pkg = packageDef(pid)
      if (pkg != h.Root)
        found += pkg
      stats foreach (body(pkg, _))
  }

  private def setPosition(d: h.Hierarchy, pos: Position) = {
    if (pos.exists) {
      pos.isSynthetic
      d.position = h.RangePosition(pos.start, pos.end)
    }
  }
}
