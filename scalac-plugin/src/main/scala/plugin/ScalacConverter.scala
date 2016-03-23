package plugin

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.Chars
import scala.tools.nsc.Global
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class ScalacConverter[G <: Global](val global: G) {
  import global.{Try ⇒ TTry, _}
  import indexer.{ hierarchy ⇒ h }

  private val found = ListBuffer[h.Hierarchy]()

  def convert(tree: Tree): Try[Seq[h.Hierarchy]] = {
    found.clear()
    Try(traverse(tree)) match {
      case Success(_) ⇒
        Success(found.toList)
      case Failure(f) ⇒
        Failure(new RuntimeException(s"Conversion of file `${tree.pos.source.file.absolute}` failed. See underlying issue for more information.", f))
    }
  }

  private def decodedName(name: Name) = {
    def addBackquotes(str: String) = {
      val (ident, op) =
        if (Chars.isIdentifierStart(str.head))
          str.span(Chars.isIdentifierPart)
        else
          ("", str)
      val needsBackticks =
        if (op.isEmpty)
          nme.keywords(name.toTermName)
        else if (!ident.isEmpty && ident.last != '_')
          true
        else
          !op.tail.forall(Chars.isOperatorPart)
      if (needsBackticks) s"`$str`" else str
    }
    addBackquotes(name.decoded.trim)
  }

  private def fullName(sym: Symbol): Seq[String] = {
    val noRootSymbol = sym.ownerChain.reverse.tail
    val noEmptyPkgSymbol = if (noRootSymbol.head.name.toTermName == nme.EMPTY_PACKAGE_NAME) noRootSymbol.tail else noRootSymbol
    noEmptyPkgSymbol.map(s ⇒ decodedName(s.name))
  }

  private def mkTypeRef(usage: h.Hierarchy, sym: Symbol): h.TypeRef = {
    val pkg = asPackageDecl(sym.owner)
    val cls = h.Decl(if (sym.name == tpnme.BYNAME_PARAM_CLASS_NAME) "Function0" else decodedName(sym.name), pkg)
    cls.addAttachments(h.ClassDecl)
    h.TypeRef(usage, cls)
  }

  // TODO handle commented references correctly
  // ignores some found references since we do want to add them as attachment to
  // other references instead of letting them live by themselves.
  private def mkTermRef(d: h.Decl, t: Tree): h.TermRef = t match {
    case Apply(fun, args) ⇒
      args foreach (expr(d, _))
      mkTermRef(d, fun)
    case Select(qualifier, selector) ⇒
      val ret = qualifier match {
        case This(qual) ⇒
          val ref = h.ThisRef(d)
          // found += ref
          ref
        case _ ⇒
          mkTermRef(d, qualifier)
      }
      val ref = h.TermRef(decodedName(selector), ret)
      // found += ref
      found += h.TermRef(decodedName(t.symbol.name), mkTypeRef(ref, t.symbol.owner))
      ref
  }

  private def mkImportRef(qualifier: Symbol, selector: Name): h.TypeRef = {
    val pkg = asPackageDecl(qualifier)
    val decl = h.Decl(decodedName(selector), pkg)
    decl.addAttachments(h.ClassDecl)
    h.TypeRef(pkg, decl)
  }

  private def typeRef(d: h.Declaration, t: Tree): Unit = t match {
    case t: TypeTree ⇒
      found += mkTypeRef(d, t.symbol)
      t.original match {
        case AppliedTypeTree(tpt, args) ⇒
          args foreach (typeRef(d, _))
        case _ ⇒
      }
  }

  private def expr(m: h.Declaration, t: Tree): Unit = t match {
    case Apply(fun, args) ⇒
      expr(m, fun)
      args foreach (expr(m, _))
    case TypeApply(fun, args) ⇒
      found += mkTypeRef(m, fun.symbol)
      args foreach (expr(m, _))
    case t: TypeTree ⇒
      typeRef(m, t)
    case Select(New(tpt), _) ⇒
      found += mkImportRef(tpt.symbol.owner, tpt.symbol.name)
    case Select(qualifier, name) ⇒
      found += mkImportRef(t.symbol.owner, name)
    case Function(vparams, body) ⇒
      vparams foreach (valDef(m, _))
      expr(m, body)
    case _: Literal ⇒
    case _: Ident ⇒
  }

  private def body(m: h.Decl, tree: Tree): Unit = tree match {
    case tree: DefDef ⇒
      defDef(m, tree)
    case tree: ValDef ⇒
      valDef(m, tree)
    case Block(stats, expr) ⇒
      stats foreach (body(m, _))
      body(m, expr)
    case _: Ident   ⇒
    case EmptyTree  ⇒
    case _: Literal ⇒
    case _: Assign  ⇒
    case tree: Apply ⇒
      expr(m, tree)
  }

  private def valDef(d: h.Declaration, t: ValDef): Unit = {
    val ValDef(_, name, tpt, rhs) = t
    if (t.symbol.isSynthetic)
      return
    val m = h.Decl(decodedName(name), d)
    m.addAttachments(if (t.symbol.isVar) h.VarDecl else h.ValDecl)
    if (t.symbol.isLazy)
      m.addAttachments(h.LazyDecl)
    found += m
    typeRef(m, tpt)
    body(m, rhs)
  }

  private def defDef(c: h.Declaration, t: DefDef): Unit = {
    val DefDef(_, name, tparams, vparamss, tpt, rhs) = t
    if (t.name == nme.CONSTRUCTOR || t.name == nme.MIXIN_CONSTRUCTOR || t.symbol.isGetter && t.symbol.isAccessor || t.symbol.isSetter)
      return
    val m = h.Decl(decodedName(name), c)
    m.addAttachments(h.DefDecl)
    found += m
    tparams foreach (typeParamDef(m, _))
    vparamss foreach (_ foreach (valDef(m, _)))
    val isGeneratedSetter = vparamss.headOption.flatMap(_.headOption).exists(_.symbol.isSetterParameter)
    if (!isGeneratedSetter)
      typeRef(m, tpt)
    body(m, rhs)
  }

  private def template(c: h.Decl, tree: Template): Unit = {
    val Template(_, _, body) = tree
    body foreach {
      case tree @ (_: ClassDef | _: ModuleDef) ⇒
        implDef(c, tree)
      case tree: DefDef ⇒
        defDef(c, tree)
      case tree: ValDef ⇒
        valDef(c, tree)
      case tree: Apply ⇒
        mkTermRef(c, tree)
        // TODO see comment at mkTermRef
        // found += mkTermRef(c, tree)
      case Select(qualifier, selector) ⇒
        found += mkImportRef(qualifier.symbol, selector)
      case EmptyTree ⇒
      case tree: Import ⇒
        implDef(c, tree)
    }
  }

  private def typeParamDef(d: h.Declaration, tree: TypeDef): Unit = {
    val TypeDef(_, name, tparams, _) = tree
    val m = h.Decl(decodedName(name), d)
    m.addAttachments(h.TypeParamDecl)
    found += m
    tparams foreach (typeParamDef(m, _))
  }

  private def implDef(decl: h.Declaration, tree: Tree): Unit = tree match {
    case ClassDef(mods, name, tparams, impl) ⇒
      val c = h.Decl(decodedName(name), decl)
      if (tree.symbol.isTrait)
        c.addAttachments(h.TraitDecl)
      else {
        c.addAttachments(h.ClassDecl)
        if (tree.symbol.isAbstract)
          c.addAttachments(h.AbstractDecl)
      }
      c.position = h.RangePosition(tree.pos.point, tree.pos.point+c.name.length)
      found += c
      tparams foreach (typeParamDef(c, _))
      template(c, impl)
    case ModuleDef(mods, name, impl) ⇒
      val c = h.Decl(decodedName(name), decl)
      c.addAttachments(h.ObjectDecl)
      c.position = h.RangePosition(tree.pos.point, tree.pos.point+c.name.length)
      found += c
      template(c, impl)
    case Import(expr, selectors) ⇒
      selectors foreach { sel ⇒
        if (sel.name == nme.WILDCARD)
          this.expr(decl, expr)
        else
          found += mkImportRef(expr.symbol, sel.name)
      }
  }

  private def mkPackageDecl(t: Tree): h.Declaration = t match {
    case Select(qualifier, name) ⇒
      val decl = h.Decl(decodedName(name), mkPackageDecl(qualifier))
      decl.addAttachments(h.PackageDecl)
      decl.position = h.RangePosition(t.pos.point, t.pos.point+decl.name.length)
      decl
    case Ident(name) if name == nme.EMPTY_PACKAGE_NAME ⇒
      h.Root
    case Ident(name) ⇒
      val decl = h.Decl(decodedName(name), h.Root)
      decl.addAttachments(h.PackageDecl)
      decl.position = h.RangePosition(t.pos.point, t.pos.point+decl.name.length)
      decl
  }

  private def asPackageDecl(sym: Symbol): h.Declaration = {
    fullName(sym) match {
      case head +: tail ⇒
        val decl = h.Decl(head, h.Root)
        decl.addAttachments(h.PackageDecl)
        tail.foldLeft(decl) { (parent, name) ⇒
          val decl = h.Decl(name, parent)
          decl.addAttachments(h.PackageDecl)
          decl
        }
      case _ ⇒
        h.Root
    }
  }

  private def traverse(tree: Tree) = tree match {
    case PackageDef(pid, stats) ⇒
      val pkg = mkPackageDecl(pid)
      if (pkg != h.Root)
        found += pkg
      stats foreach (implDef(pkg, _))
    case LabelDef(name, params, rhs)                   ⇒
    case Import(expr, selectors)                       ⇒
    case DocDef(comment, definition)                   ⇒
    case CaseDef(pat, guard, body)                     ⇒
    case Alternative(trees)                            ⇒
    case Star(elem)                                    ⇒
    case Bind(name, body)                              ⇒
    case UnApply(fun, args)                            ⇒
    case ArrayValue(elemtpt, trees)                    ⇒
    case Function(vparams, body)                       ⇒
    case Assign(lhs, rhs)                              ⇒
    case AssignOrNamedArg(lhs, rhs)                    ⇒
    case If(cond, thenp, elsep)                        ⇒
    case Match(selector, cases)                        ⇒
    case Return(expr)                                  ⇒
    case TTry(block, catches, finalizer)                ⇒
    case Throw(expr)                                   ⇒
    case New(tpt)                                      ⇒
    case Typed(expr, tpt)                              ⇒
    case TypeApply(fun, args)                          ⇒
    case ApplyDynamic(qual, args)                      ⇒
    case Super(qual, mix)                              ⇒
    case Ident(name)                                   ⇒
    case Literal(value)                                ⇒
    case Annotated(annot, arg)                         ⇒
    case SingletonTypeTree(ref)                        ⇒
    case SelectFromTypeTree(qualifier, selector)       ⇒
    case CompoundTypeTree(templ)                       ⇒
    case AppliedTypeTree(tpt, args)                    ⇒
    case TypeBoundsTree(lo, hi)                        ⇒
    case ExistentialTypeTree(tpt, whereClauses)        ⇒
    case SelectFromArray(qualifier, selector, erasure) ⇒
    case EmptyTree                                     ⇒
  }
}
