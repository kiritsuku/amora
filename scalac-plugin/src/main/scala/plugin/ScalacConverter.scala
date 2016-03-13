package plugin

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.Chars
import scala.tools.nsc.Global

class ScalacConverter[G <: Global](val global: G) {
  import global._
  import indexer.{ hierarchy ⇒ h }

  val found = ListBuffer[h.Hierarchy]()

  def findIdents(tree: Tree): Set[String] = {
    traverse(tree)
    found.map(_.toString).toSet
  }

  private def decodedName(name: Name) = {
    def addBackquotes(str: String) = {
      val (ident, op) = str.span(Chars.isScalaLetter)
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
    val pkg = h.Package(fullName(sym.owner))
    val cls = h.Class(pkg, decodedName(sym.name))
    h.TypeRef(usage, cls)
  }

  private def mkTermRef(d: h.Declaration, t: Tree): h.TermRef = t match {
    case Apply(fun, args) ⇒
      args foreach (expr(d, _))
      mkTermRef(d, fun)
    case Select(qualifier, selector) ⇒
      val ret = qualifier match {
        case This(qual) ⇒
          val pkg = h.Package(fullName(qualifier.symbol).init)
          val ref = h.ThisRef(h.Class(pkg, decodedName(qual)))
          found += ref
          ref
        case _ ⇒
          mkTermRef(d, qualifier)
      }
      val ref = h.TermRef(decodedName(selector), ret)
      found += ref
      found += h.TermRef(decodedName(t.symbol.name), mkTypeRef(ref, t.symbol.owner))
      ref
  }

  private def mkImportRef(qualifier: Symbol, selector: Name): h.TypeRef = {
    val pkg = h.Package(fullName(qualifier))
    val decl = h.Class(pkg, decodedName(selector))
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
    case Select(qualifier, name) ⇒
      found += mkImportRef(qualifier.symbol, name)
    case _ ⇒
  }

  private def body(m: h.Member, tree: Tree): Unit = tree match {
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
    val m = h.Member(d, decodedName(name))
    found += m
    typeRef(m, tpt)
    body(m, rhs)
  }

  private def defDef(c: h.Declaration, t: DefDef): Unit = {
    val DefDef(_, name, tparams, vparamss, tpt, rhs) = t
    if (t.name == nme.CONSTRUCTOR || t.name == nme.MIXIN_CONSTRUCTOR || t.symbol.isGetter && t.symbol.isAccessor)
      return
    val m = h.Member(c, decodedName(name))
    found += m
    tparams foreach (typeDef(m, _))
    vparamss foreach (_ foreach (valDef(m, _)))
    val isGeneratedSetter = vparamss.headOption.flatMap(_.headOption).exists(_.symbol.isSetterParameter)
    if (!isGeneratedSetter)
      typeRef(m, tpt)
    body(m, rhs)
  }

  private def template(c: h.Class, tree: Template): Unit = {
    val Template(_, _, body) = tree
    body foreach {
      case tree @ (_: ClassDef | _: ModuleDef) ⇒
        implDef(c, tree)
      case tree: DefDef ⇒
        defDef(c, tree)
      case tree: ValDef ⇒
        valDef(c, tree)
      case tree: Apply ⇒
        found += mkTermRef(c, tree)
      case Select(qualifier, selector) ⇒
        found += mkImportRef(qualifier.symbol, selector)
    }
  }

  private def typeDef(d: h.Declaration, tree: TypeDef): Unit = {
    val TypeDef(_, name, tparams, _) = tree
    val m = h.Member(d, decodedName(name))
    found += m
    tparams foreach (typeDef(m, _))
  }

  private def implDef(decl: h.Declaration, tree: Tree): Unit = tree match {
    case ClassDef(mods, name, tparams, impl) ⇒
      val c = h.Class(decl, decodedName(name))
      found += c
      tparams foreach (typeDef(c, _))
      template(c, impl)
    case ModuleDef(mods, name, impl) ⇒
      val c = h.Class(decl, decodedName(name))
      found += c
      template(c, impl)
    case Import(expr, selectors) ⇒
      selectors foreach { sel ⇒
        found += mkImportRef(expr.symbol, sel.name)
      }
  }

  private def traverse(tree: Tree) = tree match {
    case PackageDef(pid, stats) ⇒
      val pkg = h.Package(fullName(pid.symbol))
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
    case Try(block, catches, finalizer)                ⇒
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
