package plugin

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.Chars
import scala.tools.nsc.Global
import scala.tools.refactoring.util.SourceWithMarker
import scala.tools.refactoring.util.SourceWithMarker.Movement
import scala.tools.refactoring.util.SourceWithMarker.Movements
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

  /**
   * Decodes a `Name` to its Scala source representation. This is a necessary
   * operation because a `Name` does not represent the name of the Scala
   * identifier where it belongs to. Instead it represents the name as it is
   * seen by the compiler, which may include compiler artifacts. This function
   * takes care of these artifacts and gives back the name as it was written in
   * the Scala source.
   *
   * @param name
   *        The name that should be decoded.
   * @param nameSym
   *        The symbol where the name belongs to. Usually one wants to call this
   *        function as `decodedName(sym.name, symbol)` but in case no symbol
   *        exists, it can be called with `decodedName(name, NoSymbol)`.
   */
  private def decodedName(name: Name, nameSym: Symbol) = {
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

    val decodedName = name.decoded.trim
    if (nameSym.isLazyAccessor && nameSym.isArtifact)
      addBackquotes(decodedName.dropRight(nme.LAZY_LOCAL.length))
    else
      addBackquotes(decodedName)
  }

  private def declFromSymbol(sym: Symbol): h.Hierarchy = {
    if (sym.name.toTermName == nme.ROOT)
      h.Root
    else {
      val noRootSymbol = sym.ownerChain.reverse.tail
      val noEmptyPkgSymbol = if (noRootSymbol.head.name.toTermName == nme.EMPTY_PACKAGE_NAME) noRootSymbol.tail else noRootSymbol
      val names = noEmptyPkgSymbol.map(s ⇒ decodedName(s.name, s))
      names.foldLeft(h.Root: h.Hierarchy) { (owner, name) ⇒ h.Decl(name, owner) }
    }
  }

  private def refFromSymbol(d: h.Hierarchy, sym: Symbol): h.Ref = {
    val pkg = declFromSymbol(sym.owner)
    val cls = h.Decl(if (sym.name == tpnme.BYNAME_PARAM_CLASS_NAME) "Function0" else decodedName(sym.name, sym), pkg)
    h.Ref(cls.name, cls, d, pkg)
  }

  private def mkRef(d: h.Decl, t: Tree): h.Ref = t match {
    case Apply(fun, args) ⇒
      val ref = mkRef(d, fun)
      args foreach (expr(ref, _))
      ref
    case Select(qualifier, name) ⇒
      qualifier match {
        case _: This ⇒
        case _ ⇒
          mkRef(d, qualifier)
      }
      val calledOn = declFromSymbol(t.symbol.owner)
      val refToDecl = declFromSymbol(t.symbol)
      val ref = h.Ref(decodedName(name, NoSymbol), refToDecl, d, calledOn)
      ref.addAttachments(a.Ref)
      setPosition(ref, t.pos)
      found += ref
      ref
    case Ident(name) ⇒
      val calledOn = declFromSymbol(t.symbol.owner)
      val refToDecl = declFromSymbol(t.symbol)
      val ref = h.Ref(decodedName(name, NoSymbol), refToDecl, d, calledOn)
      ref.addAttachments(a.Ref)
      setPosition(ref, t.pos)
      found += ref
      ref
  }

  private def refFromSelect(qualifier: Symbol, name: Name): h.Ref = {
    val pkg = declFromSymbol(qualifier)
    val decl = h.Decl(decodedName(name, NoSymbol), pkg)
    decl.addAttachments(a.Class)
    val ref = h.Ref(decl.name, decl, pkg, pkg)
    ref.addAttachments(a.Ref)
    ref
  }

  private def typeRef(d: h.Hierarchy, t: Tree): Unit = t match {
    case t: TypeTree ⇒
      val sym = t.symbol

      def findTypes(t: Type): Seq[Type] =
        if (t.typeSymbol.isRefinementClass)
          t.typeSymbol.info.parents.flatMap(findTypes)
        else
          t +: t.typeArgs.flatMap(findTypes)

      def selfRefTypes() = {
        val syms = sym.info.parents.flatMap(findTypes).map(_.typeSymbol)
        val refs = syms.map(refFromSymbol(d, _))
        refs foreach (found += _)
      }

      def otherTypes() = {
        // AnyRef is de-aliased to java.lang.Object but we prefer to keep the reference to AnyRef
        val isAnyRef = t.tpe =:= typeOf[AnyRef]
        val decl = (if (isAnyRef) anyRefDecl(d) else refFromSymbol(d, sym))
        decl.addAttachments(a.Ref)
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
      found += refFromSelect(t.symbol.owner, name)
    case _: Ident ⇒
  }

  private def expr(m: h.Hierarchy, t: Tree): Unit = t match {
    case Apply(fun, args) ⇒
      expr(m, fun)
      args foreach (expr(m, _))
    case TypeApply(fun, args) ⇒
      found += refFromSymbol(m, fun.symbol)
      args foreach (expr(m, _))
    case t: TypeTree ⇒
      typeRef(m, t)
    case Select(New(tpt), _) ⇒
      found += refFromSelect(tpt.symbol.owner, tpt.symbol.name)
    case Select(_, name) ⇒
      found += refFromSelect(t.symbol.owner, name)
    case Function(vparams, body) ⇒
      vparams foreach (valDef(m, _))
      expr(m, body)
    case _: Literal ⇒
    case _: Ident ⇒
  }

  /** Handles `classOf[X]` constructs. */
  private def classOfConst(d: h.Decl, t: Literal) = t.tpe match {
    case tpe: UniqueConstantType if tpe.value.tag == ClazzTag ⇒
      val ref = refFromSymbol(d, tpe.value.typeValue.typeSymbol)
      ref.addAttachments(a.Ref)
      setPosition(ref, t.pos, skipping = Movements.commentsAndSpaces)
      found += ref

      val predef = h.Decl("Predef", h.Decl("scala", h.Root))
      val classOfRef = h.Ref("classOf", d, d, predef)
      classOfRef.addAttachments(a.Ref)
      classOfRef.position = h.RangePosition(t.pos.start, t.pos.start+classOfRef.name.length)
      found += classOfRef
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
      val decl = h.Decl(decodedName(lhs.symbol.name, lhs.symbol), m)
      body(decl, rhs)
    case tree: Apply ⇒
      expr(m, tree)
    case _: Select ⇒
      if (!(tree.symbol.isLazy && tree.symbol.isLazyAccessor))
        mkRef(m, tree)
    case _: Import ⇒
      implDef(m, tree)
  }

  private def valDef(d: h.Hierarchy, t: ValDef): Unit = {
    val ValDef(_, name, tpt, rhs) = t
    if (t.symbol.isSynthetic)
      return
    val m = h.Decl(decodedName(name, NoSymbol), d)
    m.addAttachments(if (t.symbol.isVar) a.Var else a.Val)
    if (t.symbol.isLazy)
      m.addAttachments(a.Lazy)
    if (t.symbol.isParamAccessor)
      m.addAttachments(a.Param)
    setPosition(m, t.pos)
    found += m
    typeRef(m, tpt)
    body(m, rhs)
  }

  private def defParamDef(d: h.Hierarchy, t: ValDef): Unit = {
    val ValDef(_, name, tpt, rhs) = t
    val m = h.Decl(decodedName(name, NoSymbol), d)
    m.addAttachments(a.Val, a.Param)
    setPosition(m, t.pos)
    found += m
    typeRef(m, tpt)
    body(m, rhs)
  }

  private def selfRef(d: h.Hierarchy, t: ValDef): Unit = {
    if (t == noSelfType)
      return
    val ValDef(_, name, tpt, _) = t
    val m = h.Decl(decodedName(name, NoSymbol), d)
    m.addAttachments(a.Val)
    setPosition(m, t.pos)
    found += m
    typeRef(m, tpt)
  }

  private def defDef(c: h.Decl, t: DefDef): Unit = {
    val DefDef(_, name, tparams, vparamss, tpt, rhs) = t

    def normalDefDef() = {
      val m = h.Decl(decodedName(name, NoSymbol), c)
      m.addAttachments(a.Def)
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
      val m = h.Decl(decodedName(name, NoSymbol), c)
      m.addAttachments(a.Lazy, a.Val)
      setPosition(m, t.pos)
      found += m
      typeRef(c, tpt)
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
      case tree @ (_: ClassDef | _: ModuleDef | _: Import) ⇒
        implDef(c, tree)
      case tree: DefDef ⇒
        defDef(c, tree)
      case tree: ValDef ⇒
        valDef(c, tree)
      case tree: Apply ⇒
        mkRef(c, tree)
      case t @ Select(qualifier, selector) ⇒
        val ref = refFromSelect(qualifier.symbol, selector)
        setPosition(ref, t.pos)
        found += ref
      case EmptyTree ⇒
    }
    parents foreach (typeRef(c, _))
    selfRef(c, self)
  }

  private def typeParamDef(d: h.Hierarchy, tree: TypeDef): Unit = {
    val TypeDef(_, name, tparams, _) = tree
    val m = h.Decl(decodedName(name, NoSymbol), d)
    m.addAttachments(a.TypeParam)
    found += m
    tparams foreach (typeParamDef(m, _))
  }

  private def implDef(decl: h.Hierarchy, tree: Tree): Unit = tree match {
    case ClassDef(mods, name, tparams, impl) ⇒
      val c = h.Decl(decodedName(name, NoSymbol), decl)
      if (tree.symbol.isTrait)
        c.addAttachments(a.Trait)
      else {
        c.addAttachments(a.Class)
        if (tree.symbol.isAbstract)
          c.addAttachments(a.Abstract)
      }
      setPosition(c, tree.pos)
      found += c
      tparams foreach (typeParamDef(c, _))
      template(c, impl)
    case ModuleDef(mods, name, impl) ⇒
      val c = h.Decl(decodedName(name, NoSymbol), decl)
      c.addAttachments(a.Object)
      setPosition(c, tree.pos)
      found += c
      template(c, impl)
    case Import(expr, selectors) ⇒
      selectors foreach { sel ⇒
        if (sel.name == nme.WILDCARD)
          this.expr(decl, expr)
        else {
          val ref = refFromSelect(expr.symbol, sel.name)
          ref.position = h.RangePosition(sel.namePos, sel.namePos+ref.name.length)
          found += ref

          if (sel.name != sel.rename) {
            val rename = refFromSelect(expr.symbol, sel.rename)
            rename.position = h.RangePosition(sel.renamePos, sel.renamePos+rename.name.length)
            found += rename
          }
        }
      }
  }

  private def mkPackageDecl(t: Tree): h.Hierarchy = t match {
    case Select(qualifier, name) ⇒
      val decl = h.Decl(decodedName(name, NoSymbol), mkPackageDecl(qualifier))
      decl.addAttachments(a.Package)
      setPosition(decl, t.pos)
      decl
    case Ident(name) if name == nme.EMPTY_PACKAGE_NAME ⇒
      h.Root
    case Ident(name) ⇒
      val decl = h.Decl(decodedName(name, NoSymbol), h.Root)
      decl.addAttachments(a.Package)
      setPosition(decl, t.pos)
      decl
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

  private def setPosition(d: h.Hierarchy, pos: Position, skipping: Movement = Movements.none) = {
    if (pos.isRange) {
      import Movements._
      val mvnt = until(id, skipping)
      mvnt(SourceWithMarker(pos.source.content, pos.point)) match {
        case Some(start) ⇒
          d.position = h.RangePosition(start, start+d.name.length)
        case _ ⇒
      }
    }
  }

  private def anyRefDecl(d: h.Hierarchy) = h.Ref("AnyRef", h.Decl("AnyRef", h.Decl("scala", h.Root)), d, h.Decl("scala", h.Root))
}
