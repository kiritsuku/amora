package amora.converter

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.Chars
import scala.tools.nsc.Global
import scala.tools.refactoring.util.SourceWithMarker
import scala.tools.refactoring.util.SourceWithMarker.Movement
import scala.tools.refactoring.util.SourceWithMarker.Movements
import scala.util.Failure
import scala.util.Success
import scala.util.Try

final class ScalacConverter[G <: Global](val global: G) {
  import global.{ Try ⇒ TTry, _ }
  import amora.converter.{ protocol ⇒ h }
  import amora.converter.protocol.{ Attachment ⇒ a }

  private val found = ListBuffer[h.Hierarchy]()

  /**
   * Extracts the semantic information in `tree` and converts it into a
   * structure described by [[Hierarchy]].
   */
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
   */
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

  /**
   * Computes the JVM signature for a method symbol. Throws if `sym` is not a
   * method symbol.
   */
  private def jvmSignature(sym: Symbol): String = {
    require(sym.isMethod && !sym.asMethod.isGetter, "The passed argument is not a method symbol.")

    def sig(tpe: Type): String = {
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
        case definitions.ArrayClass   ⇒ "["+sig(args.head)
        case _                        ⇒ "L"+sym.fullName.replace('.', '/')+";"
      }
    }

    val MethodType(params, ret) = sym.info.erasure
    val paramsSig = params.map(param ⇒ sig(param.info)).mkString
    val retSig = sig(if (!sym.isConstructor) ret else definitions.UnitClass.toType)
    s"($paramsSig)$retSig"
  }

  /**
   * Creates a [[Decl]] from a symbol `sym` and sets its owner to `owner.
   */
  private def mkDecl(sym: Symbol, owner: h.Hierarchy): h.Decl = {
    val name =
      if (sym.name == tpnme.BYNAME_PARAM_CLASS_NAME)
        "Function0"
      else if (sym.name == nme.CONSTRUCTOR)
        "this"
      else if (sym.isLazyAccessor && sym.isArtifact)
        decodedName(TermName(sym.name.decoded.trim.dropRight(nme.LAZY_LOCAL.length)))
      else
        decodedName(sym.name)
    val decl = h.Decl(name, owner)

    def classify(sym: Symbol): Unit = {
      if (sym.isTrait)
        decl.addAttachments(a.Trait)
      else if (sym.isClass) {
        decl.addAttachments(a.Class)
        if (sym.isAbstract)
          decl.addAttachments(a.Abstract)
      }
      else if (sym.isModule && !sym.hasPackageFlag)
        decl.addAttachments(a.Object)
      else if (sym.isLazy)
        decl.addAttachments(a.Lazy, a.Val)
      else if (sym.isMethod) {
        if (sym.asMethod.isGetter)
          classify(sym.accessed)
        else
          decl.addAttachments(a.Def, a.JvmSignature(jvmSignature(sym)))
      }
      else if (sym.isTypeParameterOrSkolem)
        decl.addAttachments(a.TypeParam)
      else if (sym.isParameter || sym.isParamAccessor)
        decl.addAttachments(if (sym.isVar) a.Var else a.Val, a.Param)
      else if (sym.isVal)
        decl.addAttachments(a.Val)
      else if (sym.isVar)
        decl.addAttachments(a.Var)
    }

    classify(sym)
    decl
  }

  /**
   * Creates a [[Decl]], whose owner is the owner of the symbol and so on untl
   * the root node is reached.
   */
  private def mkDeepDecl(sym: Symbol): h.Decl = {
    require(sym != NoSymbol, "The passed argument is NoSymbol. This is a programming error, "
        + "make sure that everything with a NoSymbol does not survive long enough to get here.")
    if (sym.name.toTermName == nme.ROOT)
      h.Root
    else {
      val noRootSymbol = sym.ownerChain.reverse.tail
      val noEmptyPkgSymbol =
        if (noRootSymbol.head.name.toTermName == nme.EMPTY_PACKAGE_NAME)
          noRootSymbol.tail
        else
          noRootSymbol

      noEmptyPkgSymbol.foldLeft(h.Root: h.Decl) { (owner, s) ⇒ mkDecl(s, owner) }
    }
  }

  private def mkRef(owner: h.Hierarchy, t: Tree): h.Ref = t match {
    case Apply(fun, args) ⇒
      val ref = mkRef(owner, fun)
      args foreach (expr(ref, _))
      ref
    case TypeApply(fun, args) ⇒
      args foreach (typeRef(owner, _))
      mkRef(owner, fun)
    case Select(New(nt), _) ⇒
      mkRef(owner, nt)
    case Select(qualifier, name) ⇒
      // implicitly called apply methods do have range positions but the position
      // of their qualifier is transparent. We need to ensure that we don't treat
      // the apply method as a range position.
      val isImplicitApplyMethod = name == nme.apply && qualifier.pos.isTransparent
      qualifier match {
        case _: This | Ident(nme.ROOTPKG) ⇒
        case Select(qualifier, nme.PACKAGE) ⇒
          mkRef(owner, qualifier)
        case _ ⇒
          mkRef(owner, qualifier)
      }
      val calledOn =
        if (t.symbol.owner.name.toTermName == nme.PACKAGE)
          mkDeepDecl(t.symbol.owner.owner)
        else
          mkDeepDecl(t.symbol.owner)
      val refToDecl = mkDecl(t.symbol, calledOn)
      // we can't use [[refToDecl.name]] here because for rename imports its name
      // is different from the name of the symbol
      val ref = h.Ref(decodedName(name), refToDecl, owner, calledOn)
      ref.addAttachments(a.Ref)
      if (!isImplicitApplyMethod)
        setPosition(ref, t.pos)
      else {
        val offset = t.pos.start
        ref.position = h.RangePosition(offset, offset)
      }
      // TODO remove this scala.Tuple check. It is a hack, which is needed as long
      // as we keep the isRange check, we actually want to get rid of it altogether.
      if (t.pos.isRange || qualifier.symbol.fullName.startsWith("scala.Tuple"))
        found += ref
      ref
    case _: Ident | _: TypeTree ⇒
      val calledOn =
        if (t.symbol.owner.isAnonymousFunction || t.symbol.owner.isLocalDummy)
          owner
        else
          mkDeepDecl(t.symbol.owner)
      val refToDecl = mkDecl(t.symbol, calledOn)
      val ref = h.Ref(refToDecl.name, refToDecl, owner, calledOn)
      ref.addAttachments(a.Ref)
      setPosition(ref, t.pos)
      if (t.pos.isRange)
        found += ref
      ref
  }

  private def typeTree(owner: h.Hierarchy, t: TypeTree): Unit = {
    val sym = t.symbol

    def refFromSymbol(sym: Symbol): h.Ref = {
      val o = mkDeepDecl(sym)
      val ref = h.Ref(o.name, o, owner, o.owner)
      ref.addAttachments(a.Ref)
      ref
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
      t.original match {
        case t: AppliedTypeTree if t.symbol.fullName.startsWith("scala.Function") && owner.position.isInstanceOf[h.RangePosition] ⇒
          // `pos.point` doesn't make a lot of sense for function literals, therefore
          // we set the position manually here
          val offset = owner.position.asInstanceOf[h.RangePosition].start
          ref.position = new h.RangePosition(offset, offset)
        case _ if owner.attachments(a.Function) && owner.position.isInstanceOf[h.RangePosition] ⇒
          // functions created through function literals don't have their positions at
          // the beginning of the identifiers where we want to have it
          val offset = owner.position.asInstanceOf[h.RangePosition].start
          ref.position = new h.RangePosition(offset, offset)
        case _ ⇒
          setPosition(ref, t.pos)
      }
      found += ref
    }

    if (sym.isRefinementClass)
      selfRefTypes
    else t.tpe match {
      case tpe: AliasTypeRef ⇒
        // ignore the underlying types of type aliases
      case tpe if !t.pos.isRange && tpe =:= typeOf[scala.annotation.Annotation] ⇒
        // Annotation is implicitly added for annotations but we only want to keep
        // it if the Annotation type is directly extended
      case tpe if !t.pos.isRange && tpe =:= typeOf[AnyRef] ⇒
        // AnyRef can leak in and we don't want to add it if it doesn't appear in source code
      case _ ⇒
        otherTypes
    }

    t.original match {
      case AppliedTypeTree(tpt, args) ⇒
        typeRef(owner, t.original)
      case t: Select ⇒
        typeRef(owner, t)
      case _ ⇒
        // TODO index other trees as well, to make for example type aliases available
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
      // TODO remove this condition because we are also interested in offset positions
      if (t.pos.isRange) {
        expr(owner, fun)
        args foreach (expr(owner, _))
      }
    case _: TypeApply ⇒
      mkRef(owner, t)
    case _: TypeTree ⇒
      typeRef(owner, t)
    case _: Select ⇒
      mkRef(owner, t)
    case Function(vparams, body) ⇒
      vparams foreach (valDef(owner, _, isFunction = true))
      expr(owner, body)
    case Bind(_, body) ⇒
      val decl = mkDecl(t.symbol, owner)
      setPosition(decl, t.pos)
      found += decl
      expr(owner, body)
    case Typed(expr, tpt) ⇒
      this.expr(owner, expr)
      typeRef(owner, tpt)
    case _: Block ⇒
      body(owner, t)
    case UnApply(fun, args) ⇒
      expr(owner, fun)
      args foreach (expr(owner, _))
    case t: Literal ⇒
      classOfConst(owner, t)
    case t: Ident ⇒
      if (t.name != nme.USCOREkw)
        mkRef(owner, t)
  }

  /** Handles `classOf[X]` constructs. */
  private def classOfConst(owner: h.Hierarchy, t: Literal) = t.tpe match {
    case tpe: UniqueConstantType if tpe.value.tag == ClazzTag ⇒
      val sym = tpe.value.typeValue.typeSymbol
      val o = mkDeepDecl(sym)
      val ref = h.Ref(o.name, o, owner, o.owner)
      ref.addAttachments(a.Ref)
      setPosition(ref, t.pos, skipping = Movements.commentsAndSpaces)
      found += ref

      val classOfRef = h.Ref("classOf", owner, owner, h.Decl("Predef", h.Decl("scala", h.Root)))
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
      val decl = mkDecl(lhs.symbol, owner)
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
    case TTry(block, catches, finalizer) ⇒
      body(owner, block)
      catches foreach (body(owner, _))
      body(owner, finalizer)
    case Throw(expr) ⇒
      body(owner, expr)
    case EmptyTree ⇒
  }

  /**
   * Handles annotations. This is rather difficult since the trees of
   * annotations are not part of the typechecked tree. Instead they are hidden
   * in the symbol of the annotated declaration. Unfortunately, these hidden
   * trees do not have range positions. Only offset positions exist in the
   * compiler generated trees. In order to handle annotations nevertheless, we
   * find the beginning of the annotation list and then parse all of the
   * annotations once again. After parsing the trees do have positions but no
   * types. Therefore we copy the positions back to the original typechecked
   * tree before we handle the trees. And at the end we remove all all copied
   * positions from the original tree in order to avoid any potential side
   * effects with later compiler phases.
   *
   * `sym` is the symbol of the declaration where the annotations belong to.
   * `pos` is the position of this declaration.
   */
  private def annotationRef(owner: h.Hierarchy, sym: Symbol, pos: Position) = {
    /* Marker that signals that we modified a tree. */
    case object NoPosAtt

    def copyPos(from: Tree, to: Tree): Unit = {
      if (to.pos == NoPosition) {
        to.updateAttachment(NoPosAtt)
        to.setPos(from.pos)
      }
      (from.children, to.children).zipped foreach copyPos
    }

    def clear(t: Tree): Unit = {
      if (t.hasAttachment[NoPosAtt.type]) {
        t.pos = NoPosition
        t.removeAttachment[NoPosAtt.type]
      }
      t.children foreach clear
    }

    val anns = sym.annotations
    if (anns.nonEmpty) {
      import scala.tools.refactoring.util.SourceWithMarker.Movements._
      val mvnt = until('@', skipping = comment | inBrackets('(', ')')).backward
      val startPos = if (sym.isParameter || sym.isParamAccessor) pos.point else pos.start
      val annPos = (1 to anns.length foldLeft startPos) { (p, _) ⇒
        mvnt(SourceWithMarker(pos.source.content, p - 1)).get
      }
      val annSrc = " "*annPos + pos.source.content.slice(annPos, startPos).mkString
      val parser = new syntaxAnalyzer.SourceFileParser(newSourceFile(annSrc, "<memory>"))
      val posTrees = parser.annotations(skipNewLines = true)
      val tpeTrees = anns.map(_.tree)

      (posTrees, tpeTrees).zipped foreach copyPos
      tpeTrees foreach (body(owner, _))
      tpeTrees foreach clear
    }
  }

  private def classDef(owner: h.Hierarchy, t: ClassDef): Unit = {
    annotationRef(owner, t.symbol, t.pos)
    val decl = mkDecl(t.symbol, owner)
    setPosition(decl, t.pos)
    found += decl
    t.tparams foreach (typeParamDef(decl, _))
    template(decl, t.impl)
  }

  private def moduleDef(owner: h.Hierarchy, t: ModuleDef): Unit = {
    annotationRef(owner, t.symbol, t.pos)
    val decl = mkDecl(t.symbol, owner)
    setPosition(decl, t.pos)
    found += decl
    template(decl, t.impl)
  }

  private def importDef(owner: h.Hierarchy, t: Import): Unit = {
    def ref(qualifier: Symbol, name: Name, pos: Int): h.Ref = {
      val decl = h.Decl(decodedName(name), mkDeepDecl(qualifier))
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

  private def valDef(owner: h.Hierarchy, t: ValDef, isFunction: Boolean = false): Unit = {
    annotationRef(owner, t.symbol, t.pos)
    if (t.symbol.isSynthetic || t.symbol.isLazy)
      return
    val decl = mkDecl(t.symbol, owner)
    if (isFunction)
      decl.addAttachments(a.Function)
    setPosition(decl, t.pos)
    found += decl
    typeRef(decl, t.tpt)
    body(decl, t.rhs)
  }

  private def selfRef(owner: h.Hierarchy, t: ValDef): Unit = {
    if (t == noSelfType)
      return
    val m = mkDecl(t.symbol, owner)
    setPosition(m, t.pos)
    found += m
    typeRef(m, t.tpt)
  }

  private def defDef(owner: h.Hierarchy, t: DefDef): Unit = {
    val DefDef(_, _, tparams, vparamss, tpt, rhs) = t

    def normalDefDef() = {
      annotationRef(owner, t.symbol, t.pos)
      val m = mkDecl(t.symbol, owner)
      setPosition(m, t.pos)
      found += m
      tparams foreach (typeParamDef(m, _))
      vparamss foreach (_ foreach (valDef(m, _)))
      val isGeneratedSetter = vparamss.headOption.flatMap(_.headOption).exists(_.symbol.isSetterParameter)
      if (!isGeneratedSetter && t.name != nme.CONSTRUCTOR)
        typeRef(m, tpt)
      body(m, rhs)
    }

    def lazyDefDef() = {
      val m = mkDecl(t.symbol, owner)
      setPosition(m, t.pos)
      found += m
      typeRef(owner, tpt)
      body(owner, rhs)
    }

    if (t.symbol.isLazy)
      lazyDefDef
    else if ((t.name == nme.CONSTRUCTOR && !t.pos.isOpaqueRange)
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
    val m = mkDecl(t.symbol, owner)
    found += m
    t.tparams foreach (typeParamDef(m, _))
  }

  private def packageDef(t: Tree): h.Decl = t match {
    case Select(qualifier, name) ⇒
      val decl = mkDecl(t.symbol, packageDef(qualifier))
      decl.addAttachments(a.Package)
      setPosition(decl, t.pos)
      decl
    case Ident(nme.EMPTY_PACKAGE_NAME) ⇒
      h.Root
    case _: Ident ⇒
      val decl = mkDecl(t.symbol, h.Root)
      decl.addAttachments(a.Package)
      setPosition(decl, t.pos)
      decl
  }

  private def traverse(t: Tree) = t match {
    case PackageDef(pid, stats) ⇒
      val pkg = packageDef(pid)
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
      import scala.tools.refactoring.util.SourceWithMarker.Movements._
      val mvnt = until(id, skipping)
      mvnt(SourceWithMarker(pos.source.content, pos.point)) match {
        case Some(start) ⇒
          d.position = h.RangePosition(start, start+d.name.length)
        case _ ⇒
      }
    } else {
      val offset = pos.start
      d.position = h.RangePosition(offset, offset)
    }
  }
}
