package plugin

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.Chars
import scala.tools.nsc.Global
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class ScalacConverter[G <: Global](val global: G) {
  import global.{ Try ⇒ TTry, _ }
  import indexer.{ hierarchy ⇒ h }
  import indexer.hierarchy.{ Attachment ⇒ a }

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
    if (sym.name.toTermName == nme.ROOT)
      Nil
    else {
      val noRootSymbol = sym.ownerChain.reverse.tail
      val noEmptyPkgSymbol = if (noRootSymbol.head.name.toTermName == nme.EMPTY_PACKAGE_NAME) noRootSymbol.tail else noRootSymbol
      noEmptyPkgSymbol.map(s ⇒ decodedName(s.name))
    }
  }

  private def mkTypeRef(usage: h.Hierarchy, sym: Symbol): h.TypeRef = {
    val pkg = asPackageDecl(sym.owner)
    val cls = h.Decl(if (sym.name == tpnme.BYNAME_PARAM_CLASS_NAME) "Function0" else decodedName(sym.name), pkg)
    cls.addAttachments(a.ClassDecl)
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
    decl.addAttachments(a.ClassDecl)
    h.TypeRef(pkg, decl)
  }

  private def typeRef(d: h.Declaration, t: Tree): Unit = t match {
    case t: TypeTree ⇒
      val sym = t.symbol

      def findTypes(t: Type): Seq[Type] =
        if (t.typeSymbol.isRefinementClass)
          t.typeSymbol.info.parents.flatMap(findTypes)
        else
          t +: t.typeArgs.flatMap(findTypes)

      def selfRefTypes() = {
        val syms = sym.info.parents.flatMap(findTypes).map(_.typeSymbol)
        val refs = syms.map(mkTypeRef(d, _))
        refs foreach (found += _)
      }

      def otherTypes() = {
        // AnyRef is de-aliased to java.lang.Object but we prefer to keep the reference to AnyRef
        val isAnyRef = t.tpe =:= typeOf[AnyRef]
        val decl = (if (isAnyRef) h.TypeRef(d, anyRefDecl) else mkTypeRef(d, sym))
        decl.addAttachments(a.TypeRef)
        setPosition(decl, t.pos)
        found += decl
        t.original match {
          case AppliedTypeTree(tpt, args) ⇒
            args foreach (typeRef(d, _))
          case _ ⇒
        }
      }

      if (sym.isRefinementClass) selfRefTypes else otherTypes
    case Select(_, name) ⇒
      found += mkImportRef(t.symbol.owner, name)
    case _: Ident ⇒
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
    case Select(_, name) ⇒
      found += mkImportRef(t.symbol.owner, name)
    case Function(vparams, body) ⇒
      vparams foreach (valDef(m, _))
      expr(m, body)
    case _: Literal ⇒
    case _: Ident ⇒
  }

  private def classOfConst(d: h.Decl, t: Literal) = t.tpe match {
    case tpe: UniqueConstantType if tpe.value.tag == ClazzTag ⇒
      val ref = mkTypeRef(d, tpe.value.typeValue.typeSymbol)
      found += ref

      val predef = h.Decl("Predef", h.Decl("scala", h.Root))
      val decl = h.TermRef("classOf", h.TypeRef(d, predef))
      found += decl
    case _ ⇒
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
    case tree: Literal ⇒
      classOfConst(m, tree)
    case Assign(lhs, rhs)  ⇒
      body(m, lhs)
      body(m, rhs)
    case tree: Apply ⇒
      expr(m, tree)
    case _: Select ⇒
  }

  private def valDef(d: h.Declaration, t: ValDef): Unit = {
    val ValDef(_, name, tpt, rhs) = t
    if (t.symbol.isSynthetic)
      return
    val m = h.Decl(decodedName(name), d)
    m.addAttachments(if (t.symbol.isVar) a.VarDecl else a.ValDecl)
    if (t.symbol.isLazy)
      m.addAttachments(a.LazyDecl)
    if (t.symbol.isParamAccessor)
      m.addAttachments(a.ParamDecl)
    setPosition(m, t.pos)
    found += m
    typeRef(m, tpt)
    body(m, rhs)
  }

  private def defParamDef(d: h.Declaration, t: ValDef): Unit = {
    val ValDef(_, name, tpt, rhs) = t
    val m = h.Decl(decodedName(name), d)
    m.addAttachments(a.ValDecl, a.ParamDecl)
    setPosition(m, t.pos)
    found += m
    typeRef(m, tpt)
    body(m, rhs)
  }

  private def selfRef(d: h.Declaration, t: ValDef): Unit = {
    if (t == noSelfType)
      return
    val ValDef(_, name, tpt, _) = t
    val m = h.Decl(decodedName(name), d)
    m.addAttachments(a.ValDecl)
    setPosition(m, t.pos)
    found += m
    typeRef(m, tpt)
  }

  private def defDef(c: h.Decl, t: DefDef): Unit = {
    val DefDef(_, name, tparams, vparamss, tpt, rhs) = t

    def normalDefDef() = {
      val m = h.Decl(decodedName(name), c)
      m.addAttachments(a.DefDecl)
      setPosition(m, t.pos)
      found += m
      tparams foreach (typeParamDef(m, _))
      vparamss foreach (_ foreach (defParamDef(m, _)))
      val isGeneratedSetter = vparamss.headOption.flatMap(_.headOption).exists(_.symbol.isSetterParameter)
      if (!isGeneratedSetter)
        typeRef(m, tpt)
      body(m, rhs)
    }

    def lazyDefDef() = {
      body(c, rhs)
    }

    if (t.symbol.isLazy)
      lazyDefDef
    else if (t.name == nme.CONSTRUCTOR
          || t.name == nme.MIXIN_CONSTRUCTOR
          || t.symbol.isGetter && t.symbol.isAccessor
          || t.symbol.isSetter)
      ()
    else
      normalDefDef
  }

  private def template(c: h.Decl, tree: Template): Unit = {
    val Template(parents, self, body) = tree
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
    parents foreach (typeRef(c, _))
    selfRef(c, self)
  }

  private def typeParamDef(d: h.Declaration, tree: TypeDef): Unit = {
    val TypeDef(_, name, tparams, _) = tree
    val m = h.Decl(decodedName(name), d)
    m.addAttachments(a.TypeParamDecl)
    found += m
    tparams foreach (typeParamDef(m, _))
  }

  private def implDef(decl: h.Declaration, tree: Tree): Unit = tree match {
    case ClassDef(mods, name, tparams, impl) ⇒
      val c = h.Decl(decodedName(name), decl)
      if (tree.symbol.isTrait)
        c.addAttachments(a.TraitDecl)
      else {
        c.addAttachments(a.ClassDecl)
        if (tree.symbol.isAbstract)
          c.addAttachments(a.AbstractDecl)
      }
      setPosition(c, tree.pos)
      found += c
      tparams foreach (typeParamDef(c, _))
      template(c, impl)
    case ModuleDef(mods, name, impl) ⇒
      val c = h.Decl(decodedName(name), decl)
      c.addAttachments(a.ObjectDecl)
      setPosition(c, tree.pos)
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
      decl.addAttachments(a.PackageDecl)
      setPosition(decl, t.pos)
      decl
    case Ident(name) if name == nme.EMPTY_PACKAGE_NAME ⇒
      h.Root
    case Ident(name) ⇒
      val decl = h.Decl(decodedName(name), h.Root)
      decl.addAttachments(a.PackageDecl)
      setPosition(decl, t.pos)
      decl
  }

  private def asPackageDecl(sym: Symbol): h.Declaration = {
    fullName(sym) match {
      case head +: tail ⇒
        val decl = h.Decl(head, h.Root)
        decl.addAttachments(a.PackageDecl)
        tail.foldLeft(decl) { (parent, name) ⇒
          val decl = h.Decl(name, parent)
          decl.addAttachments(a.PackageDecl)
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

  private def setPosition(d: h.Hierarchy, pos: Position) = {
    d.position = h.RangePosition(pos.point, pos.point+d.name.length)
  }

  private def anyRefDecl = h.Decl("AnyRef", h.Decl("scala", h.Root))
}
