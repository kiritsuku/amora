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
    Try(packageDef(tree)) match {
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

  def signature(sym: Symbol): String = {
    require(sym.isMethod && !sym.asMethod.isGetter, "The passed argument is not a method symbol.")

    def jvmSignature(tpe: Type): String = {
      val TypeRef(_, sym, args) = tpe
      sym match {
        case definitions.UnitClass    ⇒ "V"
        case definitions.BooleanClass ⇒ "Z"
        case definitions.CharClass    ⇒ "C"
        case definitions.ByteClass    ⇒ "B"
        case definitions.ShortClass   ⇒ "S"
        case definitions.IntClass     ⇒ "I"
        case definitions.FloatClass   ⇒ "F"
        case definitions.LongClass    ⇒ "J"
        case definitions.DoubleClass  ⇒ "D"
        case definitions.ArrayClass   ⇒ "["+jvmSignature(args.head)
        case _                        ⇒ "L"+sym.fullName.replace('.', '/')+";"
      }
    }

    val MethodType(params, ret) = sym.info.erasure
    val paramsSig = params.map(param ⇒ jvmSignature(param.info)).mkString
    val retSig = jvmSignature(if (!sym.isConstructor) ret else definitions.UnitClass.toType)
    s"($paramsSig)$retSig"
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

  private def mkRef(owner: h.Hierarchy, t: Tree): h.Ref = t match {
    case Apply(fun, args) ⇒
      val ref = mkRef(owner, fun)
      args foreach (expr(ref, _))
      ref
    case TypeApply(fun, args) ⇒
      val sym = fun.symbol
      val cls = h.Decl(decodedName(sym.name, sym), declFromSymbol(sym.owner))
      val ref = h.Ref(cls.name, cls, owner, cls.owner)
      ref.addAttachments(a.Ref)
      setPosition(ref, t.pos)
      found += ref
      args foreach (expr(owner, _))
      ref
    case Select(New(t), _) ⇒
      val calledOn = declFromSymbol(t.symbol.owner)
      val refToDecl = declFromSymbol(t.symbol)
      val ref = h.Ref(refToDecl.name, refToDecl, owner, calledOn)
      ref.addAttachments(a.Ref)
      setPosition(ref, t.pos)
      if (t.pos.isRange)
        found += ref
      ref
    case Select(qualifier, name) ⇒
      qualifier match {
        case _: This ⇒
        case Ident(name) if name == nme.ROOTPKG ⇒
        case _ ⇒
          mkRef(owner, qualifier)
      }
      val calledOn = declFromSymbol(t.symbol.owner)
      val refToDecl = declFromSymbol(t.symbol)
      val ref = h.Ref(decodedName(name, NoSymbol), refToDecl, owner, calledOn)
      ref.addAttachments(a.Ref)
      setPosition(ref, t.pos)
      if (t.pos.isRange)
        found += ref
      ref
    case Ident(name) ⇒
      val calledOn = declFromSymbol(t.symbol.owner)
      val refToDecl = declFromSymbol(t.symbol)
      val ref = h.Ref(decodedName(name, NoSymbol), refToDecl, owner, calledOn)
      ref.addAttachments(a.Ref)
      setPosition(ref, t.pos)
      if (t.pos.isRange)
        found += ref
      ref
  }

  private def typeTree(owner: h.Hierarchy, t: TypeTree): Unit = {
    val sym = t.symbol

    def refFromSymbol(sym: Symbol): h.Ref = {
      val pkg = declFromSymbol(sym.owner)
      val cls = h.Decl(if (sym.name == tpnme.BYNAME_PARAM_CLASS_NAME) "Function0" else decodedName(sym.name, sym), pkg)
      h.Ref(cls.name, cls, owner, pkg)
    }

    def selfRefTypes() = {
      def findTypes(t: Type): Seq[Type] =
        if (t.typeSymbol.isRefinementClass)
          t.typeSymbol.info.parents.flatMap(findTypes)
        else
          t +: t.typeArgs.flatMap(findTypes)

      val syms = sym.info.parents.flatMap(findTypes).map(_.typeSymbol)
      val refs = syms.map(refFromSymbol)
      refs foreach (found += _)
    }

    def otherTypes() = {
      val ref = refFromSymbol(sym)
      ref.addAttachments(a.Ref)
      setPosition(ref, t.pos)
      // AnyRef can leak in and we don't want to add it if it doesn't appear in source code
      val isImplicitAnyRef = t.tpe =:= typeOf[AnyRef] && !t.pos.isRange
      if (!isImplicitAnyRef)
        found += ref
    }

    if (sym.isRefinementClass)
      selfRefTypes
    else
      otherTypes

    t.original match {
      case AppliedTypeTree(tpt, args) ⇒
        typeRef(owner, t.original)
      case _ ⇒
    }
  }

  private def typeRef(owner: h.Hierarchy, t: Tree): Unit = t match {
    case t: TypeTree ⇒
      typeTree(owner, t)
    case AppliedTypeTree(tpt, args) ⇒
      if (tpt.symbol.name != tpnme.BYNAME_PARAM_CLASS_NAME)
        typeRef(owner, tpt)
      args.filter(_.symbol != NoSymbol) foreach (typeRef(owner, _))
    case _: Select ⇒
      mkRef(owner, t)
    case _: Ident ⇒
      mkRef(owner, t)
  }

  private def expr(owner: h.Hierarchy, t: Tree): Unit = t match {
    case Apply(fun, args) ⇒
      expr(owner, fun)
      args foreach (expr(owner, _))
    case _: TypeApply ⇒
      mkRef(owner, t)
    case t: TypeTree ⇒
      typeRef(owner, t)
    case _: Select ⇒
      mkRef(owner, t)
    case Function(vparams, body) ⇒
      vparams foreach (valDef(owner, _))
      expr(owner, body)
    case Bind(name, body) ⇒
      val decl = h.Decl(decodedName(name, NoSymbol), owner)
      setPosition(decl, t.pos)
      found += decl
      expr(owner, body)
    case Typed(expr, tpt) ⇒
      this.expr(owner, expr)
      typeRef(owner, tpt)
    case _: Literal ⇒
    case _: Ident ⇒
  }

  /** Handles `classOf[X]` constructs. */
  private def classOfConst(owner: h.Hierarchy, t: Literal) = t.tpe match {
    case tpe: UniqueConstantType if tpe.value.tag == ClazzTag ⇒
      val sym = tpe.value.typeValue.typeSymbol
      val cls = h.Decl(decodedName(sym.name, sym), declFromSymbol(sym.owner))
      val ref = h.Ref(cls.name, cls, owner, cls.owner)
      ref.addAttachments(a.Ref)
      setPosition(ref, t.pos, skipping = Movements.commentsAndSpaces)
      found += ref

      val predef = h.Decl("Predef", h.Decl("scala", h.Root))
      val classOfRef = h.Ref("classOf", owner, owner, predef)
      classOfRef.addAttachments(a.Ref)
      classOfRef.position = h.RangePosition(t.pos.start, t.pos.start+classOfRef.name.length)
      found += classOfRef
    case _ ⇒
  }

  private def body(owner: h.Hierarchy, t: Tree): Unit = t match {
    case t: DefDef ⇒
      defDef(owner, t)
    case t: ValDef ⇒
      valDef(owner, t)
    case Block(stats, expr) ⇒
      stats foreach (body(owner, _))
      body(owner, expr)
    case t: Literal ⇒
      classOfConst(owner, t)
    case Assign(lhs, rhs) ⇒
      body(owner, lhs)
      val decl = h.Decl(decodedName(lhs.symbol.name, lhs.symbol), owner)
      body(decl, rhs)
    case t: Apply ⇒
      mkRef(owner, t)
    case t: Select ⇒
      if (!(t.symbol.isLazy && t.symbol.isLazyAccessor))
        mkRef(owner, t)
    case t: ClassDef ⇒
      classDef(owner, t)
    case t: ModuleDef ⇒
      moduleDef(owner, t)
    case t: Import ⇒
      importDef(owner, t)
    case _: Ident ⇒
      mkRef(owner, t)
    case If(cond, thenp, elsep) ⇒
      body(owner, cond)
      body(owner, thenp)
      body(owner, elsep)
    case Match(selector, cases) ⇒
      body(owner, selector)
      // TODO we need to put the case expressions into a new owner to make variable definitions unique
      cases foreach (body(owner, _))
    case CaseDef(pat, guard, body) ⇒
      expr(owner, pat)
      this.body(owner, guard)
      this.body(owner, body)
    case EmptyTree ⇒
  }

  private def classDef(owner: h.Hierarchy, t: ClassDef): Unit = {
    val ClassDef(_, name, tparams, impl) = t
    val c = h.Decl(decodedName(name, NoSymbol), owner)
    if (t.symbol.isTrait)
      c.addAttachments(a.Trait)
    else {
      c.addAttachments(a.Class)
      if (t.symbol.isAbstract)
        c.addAttachments(a.Abstract)
    }
    setPosition(c, t.pos)
    found += c
    tparams foreach (typeParamDef(c, _))
    template(c, impl)
  }

  private def moduleDef(owner: h.Hierarchy, t: ModuleDef): Unit = {
    val ModuleDef(_, name, impl) = t
    val c = h.Decl(decodedName(name, NoSymbol), owner)
    c.addAttachments(a.Object)
    setPosition(c, t.pos)
    found += c
    template(c, impl)
  }

  private def importDef(owner: h.Hierarchy, t: Import): Unit = {
    def ref(qualifier: Symbol, name: Name, pos: Int): h.Ref = {
      val decl = h.Decl(decodedName(name, NoSymbol), declFromSymbol(qualifier))
      val ref = h.Ref(decl.name, decl, decl.owner, decl.owner)
      ref.addAttachments(a.Ref)
      ref.position = h.RangePosition(pos, pos+ref.name.length)
      ref
    }

    val Import(qualifier, selectors) = t
    mkRef(owner, qualifier)
    selectors foreach { sel ⇒
      if (sel.name != nme.WILDCARD) {
        found += ref(qualifier.symbol, sel.name, sel.namePos)

        if (sel.name != sel.rename)
          found += ref(qualifier.symbol, sel.rename, sel.renamePos)
      }
    }
  }

  private def valDef(owner: h.Hierarchy, t: ValDef): Unit = {
    if (t.symbol.isSynthetic)
      return
    val ValDef(_, name, tpt, rhs) = t
    val m = h.Decl(decodedName(name, NoSymbol), owner)
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

  private def defParamDef(owner: h.Hierarchy, t: ValDef): Unit = {
    val ValDef(_, name, tpt, rhs) = t
    val m = h.Decl(decodedName(name, NoSymbol), owner)
    m.addAttachments(a.Val, a.Param)
    setPosition(m, t.pos)
    found += m
    typeRef(m, tpt)
    body(m, rhs)
  }

  private def selfRef(owner: h.Hierarchy, t: ValDef): Unit = {
    if (t == noSelfType)
      return
    val ValDef(_, name, tpt, _) = t
    val m = h.Decl(decodedName(name, NoSymbol), owner)
    m.addAttachments(a.Val)
    setPosition(m, t.pos)
    found += m
    typeRef(m, tpt)
  }

  private def defDef(owner: h.Hierarchy, t: DefDef): Unit = {
    val DefDef(_, name, tparams, vparamss, tpt, rhs) = t

    def normalDefDef() = {
      val m = h.Decl(decodedName(name, NoSymbol), owner)
      m.addAttachments(a.Def)
      m.addAttachments(a.JvmSignature(signature(t.symbol)))
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
      val m = h.Decl(decodedName(name, NoSymbol), owner)
      m.addAttachments(a.Lazy, a.Val)
      setPosition(m, t.pos)
      found += m
      typeRef(owner, tpt)
      body(owner, rhs)
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

  private def template(owner: h.Hierarchy, t: Template): Unit = {
    val Template(parents, self, body) = t
    body foreach (this.body(owner, _))
    parents foreach (typeRef(owner, _))
    selfRef(owner, self)
  }

  private def typeParamDef(owner: h.Hierarchy, t: TypeDef): Unit = {
    val TypeDef(_, name, tparams, _) = t
    val m = h.Decl(decodedName(name, NoSymbol), owner)
    m.addAttachments(a.TypeParam)
    found += m
    tparams foreach (typeParamDef(m, _))
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

  private def packageDef(t: Tree) = t match {
    case PackageDef(pid, stats) ⇒
      val pkg = mkPackageDecl(pid)
      if (pkg != h.Root)
        found += pkg
      stats foreach (body(pkg, _))
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
}
