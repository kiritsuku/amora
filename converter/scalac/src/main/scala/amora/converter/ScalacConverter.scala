package amora.converter

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.Chars
import scala.reflect.io.AbstractFile
import scala.tools.nsc.Global
import scala.tools.refactoring.util.SourceWithMarker
import scala.tools.refactoring.util.SourceWithMarker.Movement
import scala.tools.refactoring.util.SourceWithMarker.Movements
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import amora.converter.{ protocol ⇒ h }
import amora.converter.protocol.{ Attachment ⇒ a }

final class ScalacConverter[G <: Global](
    val global: G,
    addDeclAttachment: (G#Symbol, h.Decl) ⇒ Unit = (_: G#Symbol, _: h.Decl) ⇒ (),
    addRefAttachment: (AbstractFile, h.Ref) ⇒ Unit = (_, _) ⇒ ()) {
  import global.{ Try ⇒ TTry, _ }

  private val found = ListBuffer[h.Hierarchy]()
  private var scopes = Scopes()

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
        Failure(new IllegalStateException(
            s"Conversion of file `${tree.pos.source.file.absolute}` failed." +
            " This is a bug, please report it at `https://github.com/sschaef/amora/issues`." +
            " See underlying issue for more information.", f))
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
        case _                        ⇒ "L"+sym.javaBinaryNameString+";"
      }
    }

    val MethodType(params, ret) = sym.info.erasure
    val paramsSig = params.map(param ⇒ sig(param.info)).mkString
    val retSig = sig(if (!sym.isConstructor) ret else definitions.UnitClass.toType)
    s"($paramsSig)$retSig"
  }

  private def requireSymbolDefined(sym: Symbol) =
    require(sym != NoSymbol, "The passed argument is NoSymbol. This is a programming error, "
        + "make sure that everything with a NoSymbol does not survive long enough to get here.")

  private def mkName(sym: Symbol): String = {
    requireSymbolDefined(sym)
    if (sym.name == tpnme.BYNAME_PARAM_CLASS_NAME)
      "Function0"
    else if (sym.name == nme.CONSTRUCTOR)
      "this"
    else if (sym.isLazyAccessor && sym.isArtifact)
      decodedName(TermName(sym.name.decoded.trim.dropRight(nme.LAZY_LOCAL.length)))
    else
      decodedName(sym.name)
  }

  private def classifyDecl(sym: Symbol, decl: h.Decl): Unit = {
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
        classifyDecl(sym.accessed, decl)
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
    else if (sym.hasPackageFlag)
      decl.addAttachments(a.Package)

    addDeclAttachment(sym, decl)
  }

  /**
   * Creates a [[Decl]] from a symbol `sym` and sets its owner to `owner`.
   */
  private def mkDecl(sym: Symbol, owner: h.Hierarchy): h.Decl = {
    requireSymbolDefined(sym)
    val decl = h.Decl(mkName(sym), owner)
    classifyDecl(sym, decl)
    decl
  }

  /**
   * Similar to [[mkDecl]] but also assigns a position to the [[Decl]]. The
   * position is set before [[classifyDecl]] is called in order to make it
   * possible to correctly attach the source file to the [[Decl]].
   */
  private def mkDeclWithPos(sym: Symbol, owner: h.Hierarchy, pos: Position): h.Decl = {
    requireSymbolDefined(sym)
    val decl = h.Decl(mkName(sym), owner)
    setPosition(decl, pos)
    classifyDecl(sym, decl)
    decl
  }

  /**
   * Creates a [[Decl]], whose owner is the owner of the symbol and so on until
   * the root node is reached.
   */
  private def mkDeepDecl(sym: Symbol): h.Decl = {
    requireSymbolDefined(sym)
    if (sym.name.toTermName == nme.ROOT)
      h.Root
    else {
      val noRootSymbol = sym.ownerChain.reverse.tail
      val noEmptyPkgSymbol =
        if (noRootSymbol.head.name.toTermName == nme.EMPTY_PACKAGE_NAME)
          noRootSymbol.tail
        else
          noRootSymbol
      val noPackageObjects = noEmptyPkgSymbol.filter(!_.isPackageObjectClass)

      noPackageObjects.foldLeft(h.Root: h.Decl) { (owner, s) ⇒ mkDecl(s, owner) }
    }
  }

  private def mkRef(t: Tree, name: String, refToDecl: h.Hierarchy, owner: h.Hierarchy): h.Ref = {
    require(refToDecl.attachments.exists(_.isInstanceOf[a.SourceFile]),
        s"No SourceFile attachment for `$refToDecl` in tree `$t` found.")
    val ref = h.Ref(name, refToDecl, owner, None)
    ref.addAttachments(a.Ref)
    addRefAttachment(t.pos.source.file, ref)
    ref
  }

  private def mkRef(t: Tree, name: String, refToDecl: h.Hierarchy, owner: h.Hierarchy, calledOn: h.Hierarchy): h.Ref = {
    require(refToDecl.attachments.exists(_.isInstanceOf[a.SourceFile]),
        s"No SourceFile attachment for `$refToDecl` in tree `$t` found.")
    val ref = h.Ref(name, refToDecl, owner, Some(calledOn))
    ref.addAttachments(a.Ref)
    addRefAttachment(t.pos.source.file, ref)
    ref
  }

  /**
   * `isTopLevelRef` should be `true` when this method is called. It can be set
   * to `false` when this method calls itself recursively. We need to know if a
   * ref is top level for Selects. `a.b.c` is a Select and it can exist
   * explicitly in the sources or implicitly to qualify a symbol. In the latter
   * case we always want to throw away inner selects like `a.b` and `a` but
   * sometimes we want to keep the top level select `a.b.c`. This may be the
   * case for implicitly called `apply` methods as in `Option(1)`, where the
   * Select would be `scala.Option.apply` (`apply` is kept but `scala.Option`
   * and `scala` are thrown away).
   */
  private def refTree(owner: h.Hierarchy, tree: Tree, isTopLevelRef: Boolean = true): h.Ref = tree match {
    case Apply(fun, args) ⇒
      val ref = refTree(owner, fun, isTopLevelRef)
      args foreach (body(ref, _))
      ref
    case TypeApply(fun, args) ⇒
      args foreach (typeRef(owner, _))
      refTree(owner, fun, isTopLevelRef)
    case Select(New(nt), _) ⇒
      refTree(owner, nt, isTopLevelRef)
    case t @ Select(qualifier, name) ⇒
      val qualifierRef = qualifier match {
        case _: This | Ident(nme.ROOTPKG) | _: Super ⇒
          None
        case Select(qualifier, nme.PACKAGE) ⇒
          Some(refTree(owner, qualifier, isTopLevelRef = false))
        case _ ⇒
          Some(refTree(owner, qualifier, isTopLevelRef = false))
      }
      val refToDecl = {
        val ownerDecl =
          if (t.symbol.owner.name.toTermName == nme.PACKAGE)
            mkDeepDecl(t.symbol.owner.owner)
          else
            mkDeepDecl(t.symbol.owner)
        mkDeclWithPos(t.symbol, ownerDecl, t.pos)
      }
      val refName =
        if (name == nme.CONSTRUCTOR)
          "this"
        else
          decodedName(name)
      // we have to use `refName` instead of `refToDecl.name` here because for
      // rename imports its name is different from the name of the symbol
      val ref = qualifierRef match {
        case Some(qualifierRef) ⇒
          val ref = mkRef(t, refName, refToDecl, owner, qualifierRef)
          qualifierRef.attachments.collectFirst {
            case a.Order(nr) ⇒
              ref.addAttachments(a.Order(nr + 1))
          }
          ref
        case None ⇒
          val ref = mkRef(t, refName, refToDecl, owner)
          ref.addAttachments(a.Order(1))
          ref
      }

      // implicitly called apply methods do have range positions but the position
      // of their qualifier is transparent. We need to ensure that we don't treat
      // the apply method as a range position.
      def isImplicitApplyMethod = name == nme.apply && qualifier.pos.isTransparent
      // some refs are implicitly added by the compiler (like `scala.AnyRef` parents
      // for classes). In this case they don't have a range position but they are top
      // level and their only reasonable position can be the offset of their owners.
      if (!t.pos.isRange)
        setPositionOfOwner(owner, ref)
      else if (!isImplicitApplyMethod)
        setPosition(ref, t.pos)
      else {
        val offset = t.pos.start
        ref.position = h.RangePosition(offset, offset)
      }

      if (isTopLevelRef || t.pos.isRange)
        found += ref
      ref
    case _: Ident | _: TypeTree | _: This ⇒
      val t = tree match {
        case t: TypeTree if t.original != null ⇒ t.original
        case t ⇒ t
      }

      def mkIdent = {
        val d = h.Decl(mkName(t.symbol), h.Root)
        classifyDecl(t.symbol, d)
        d.asString
      }
      val calledOn =
        if (t.symbol.owner.isAnonymousFunction || t.symbol.owner.isLocalDummy)
          owner
        else scopes.asDecl(mkIdent) match {
          case Some(decl) ⇒ decl.owner
          case None ⇒ mkDeepDecl(t.symbol.owner)
        }
      val rawRefToDecl = mkDeclWithPos(t.symbol, calledOn, t.pos)
      val (n, refToDecl) = t match {
        // the names of self refs are not part of the tree. They are in the same
        // way represented as a this reference. We have to check the source code
        // to find out what it is.
        case _: This ⇒
          val thisRef = t.pos.source.content.slice(t.pos.start, t.pos.end).mkString
          if (thisRef == "this")
            "this" → rawRefToDecl
          else {
            // we need to create the referenced decl manually here because I don't
            // know how to get access to the symbol of the self ref.
            val refToDecl = h.Decl(thisRef, rawRefToDecl)
            refToDecl.addAttachments(a.Val)
            addDeclAttachment(t.symbol, refToDecl)
            thisRef → refToDecl
          }
        case _ ⇒
          rawRefToDecl.name → rawRefToDecl
      }
      val ref = mkRef(t, n, refToDecl, owner, calledOn)
      t match {
        // we need to manually adjust positions for `this` references because
        // the implementation of `setPosition` for some reason can't handle them.
        case _: This ⇒
          ref.position = h.RangePosition(t.pos.point, t.pos.point+n.length)
        case _ ⇒
          setPosition(ref, t.pos)
      }
      if (t.pos.isRange)
        found += ref
      ref
    case t: Literal ⇒
      val ref = classOfConst(owner, t).getOrElse {
        throwTreeMatchError(t)
      }
      found += ref
      ref
    case t ⇒
      throwTreeMatchError(t)
  }

  private def setPositionOfOwner(owner: h.Hierarchy, elem: h.Hierarchy): Unit = {
    owner.position match {
      case h.RangePosition(offset, _) ⇒
        elem.position = h.RangePosition(offset, offset)
      case _ ⇒
    }
  }

  /**
   * `selfRefPos` needs to be set when the type tree belongs to the type of a
   * self reference. The value in the `Option` is the start offset of the self
   * reference.
   */
  private def typeTree(owner: h.Hierarchy, t: TypeTree, selfRefPos: Option[Int], isRepeatedArg: Boolean): Unit = {
    def refFromSymbol(sym: Symbol): h.Ref = {
      val o = mkDeepDecl(sym)
      mkRef(t, o.name, o, owner, o.owner)
    }

    def selfRefTypes() = {
      def findTypes(t: Type): Seq[Type] =
        if (t.typeSymbol.isRefinementClass)
          t.typeSymbol.info.parents.flatMap(findTypes)
        else
          t +: t.typeArgs.flatMap(findTypes)

      def findTrees(t: Tree): Seq[Tree] = t match {
        case CompoundTypeTree(Template(parents, _, _)) ⇒
          parents.flatMap(findTrees)
        case AppliedTypeTree(tpt, args) ⇒
          tpt +: args.flatMap(findTrees)
        case t ⇒
          Seq(t)
      }

      val typeParents = t.symbol.info.parents.flatMap(findTypes)
      val treeParents = findTrees(t.original)

      val selfRef = refFromSymbol(typeParents.head.typeSymbol)
      selfRefPos foreach { offset ⇒
        selfRef.position = h.RangePosition(offset, offset)
      }
      found += selfRef

      typeParents.tail zip treeParents foreach {
        case (typeParent, treeParent) ⇒
          val ref = refFromSymbol(typeParent.typeSymbol)
          setPosition(ref, treeParent.pos)
          found += ref
      }
    }

    if (t.symbol.isRefinementClass)
      selfRefTypes
    else t.tpe match {
      case tpe: AliasTypeRef ⇒
        val ref = refFromSymbol(tpe.sym)
        setPosition(ref, t.pos)
        found += ref
      case tpe if !t.pos.isRange && tpe =:= typeOf[scala.annotation.Annotation] ⇒
        // Annotation is implicitly added for annotations but we only want to keep
        // it if the Annotation type is directly extended
      case tpe if !t.pos.isRange && tpe =:= typeOf[AnyRef] ⇒
        // AnyRef can leak in and we don't want to add it if it doesn't appear in source code
      case _ if t.symbol.name == tpnme.REPEATED_PARAM_CLASS_NAME ⇒
        // Ignore repeated arguments
      case _ ⇒
        val ref = refFromSymbol(t.symbol)
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
            // the position for self references are wrong, we had to pass it as a
            // parameter to this function
            selfRefPos match {
              case Some(offset) ⇒
                ref.position = h.RangePosition(offset, offset)
              case None ⇒
                setPosition(ref, t.pos)
            }
        }
        if (isRepeatedArg)
          ref.addAttachments(a.Repeated)
        found += ref
    }

    t.original match {
      case t: AppliedTypeTree ⇒
        typeRef(owner, t)
      case t: Select ⇒
        typeRef(owner, t)
      // Compound type trees need special handling, since their original representation
      // lacks symbols.
      case t @ CompoundTypeTree(Template(parents, _, _)) ⇒
        // we need to extract symbols from the types and later combine the symbols
        // with the trees because the trees do not contain the symbols.
        def handleTypeArguments() = {
          def findTypes(t: Type): Seq[Type] =
            t +: t.typeArgs.flatMap(findTypes)

          def findTrees(t: Tree): Seq[Tree] = t match {
            case AppliedTypeTree(tpt, args) ⇒
              tpt +: args.flatMap(findTrees)
            case t ⇒
              Seq(t)
          }

          val typeParents = t.tpe.parents.flatMap(findTypes)
          val treeParents = parents.flatMap(findTrees)
          typeParents zip treeParents foreach {
            case (typeParent, treeParent) ⇒
              val ref = refFromSymbol(typeParent.typeSymbol)
              setPosition(ref, treeParent.pos)
              found += ref
          }
        }

        // Only the Select trees from the qualifiers contain symbols.
        def handleQualifiers() = {
          parents foreach {
            case AppliedTypeTree(tpt: Select, _) if tpt.symbol != NoSymbol ⇒
              typeRef(owner, tpt)
            case _ ⇒
          }
        }

        handleTypeArguments()
        handleQualifiers()
      case _ ⇒
    }
  }

  /**
   * See [[typeTree]] for information about what `selfRefPos` is doing.
   */
  private def typeRef(owner: h.Hierarchy, t: Tree, selfRefPos: Option[Int] = None, isRepeatedArg: Boolean = false): Unit = t match {
    case t: TypeTree ⇒
      typeTree(owner, t, selfRefPos, isRepeatedArg)
    case AppliedTypeTree(tpt, args) ⇒
      val n = tpt.symbol.name
      if (n == tpnme.REPEATED_PARAM_CLASS_NAME)
        typeRef(owner, args.head, selfRefPos, isRepeatedArg = true)
      else {
        if (n != tpnme.BYNAME_PARAM_CLASS_NAME)
          typeRef(owner, tpt)
        args.filter(_.symbol != NoSymbol) foreach (typeRef(owner, _))
      }
    case _: Select ⇒
      refTree(owner, t)
    case _: Ident ⇒
      refTree(owner, t)
    case t ⇒
      throwTreeMatchError(t)
  }

  /** Handles `classOf[X]` constructs. */
  private def classOfConst(owner: h.Hierarchy, t: Literal): Option[h.Ref] = t.tpe match {
    case tpe: UniqueConstantType if tpe.value.tag == ClazzTag ⇒
      val sym = tpe.value.typeValue.typeSymbol
      val o = mkDeepDecl(sym)
      classifyDecl(sym, o)
      val ref = mkRef(t, o.name, o, owner, o.owner)
      setPosition(ref, t.pos, skipping = Movements.commentsAndSpaces)
      found += ref

      // the symbol of the tree is `null`, we have to get it from elsewhere
      val classOfSymbol = global.currentRun.runDefinitions.Predef_classOf
      val refToDecl = mkDeepDecl(classOfSymbol)
      classifyDecl(classOfSymbol, refToDecl)
      val classOfRef = mkRef(t ,"classOf", refToDecl, owner, refToDecl.owner)
      classOfRef.position = h.RangePosition(t.pos.start, t.pos.start+classOfRef.name.length)
      found += classOfRef
      Some(classOfRef)
    case _ ⇒
      None
  }

  private def body(owner: h.Hierarchy, t: Tree, codeOrder: Int = 0): Unit = t match {
    case t: DefDef ⇒
      defDef(owner, t)
    case t: ValDef ⇒
      valDef(owner, t, codeOrder)
    case t: TypeDef ⇒
      typeDef(owner, t)
    case Block(stats, expr) ⇒
      withNewScope {
        stats.zipWithIndex foreach { case (s, i) ⇒ body(owner, s, i + 1) }
        body(owner, expr, stats.length + 1)
      }
    case t: Literal ⇒
      classOfConst(owner, t)
    case Assign(lhs, rhs) ⇒
      body(owner, lhs)
      val decl = mkDecl(lhs.symbol, owner)
      body(decl, rhs)
    case t: Select ⇒
      if (!(t.symbol.isLazy && t.symbol.isLazyAccessor))
        refTree(owner, t)
    case t: ClassDef ⇒
      classDef(owner, t)
    case t: ModuleDef ⇒
      moduleDef(owner, t)
    case t: Import ⇒
      importDef(owner, t)
    case If(cond, thenp, elsep) ⇒
      withKeywordScope(owner, t, a.If) { sIf ⇒
        body(sIf, cond)
        body(sIf, thenp)
      }

      elsep match {
        case Literal(c) if c.tag == UnitTag ⇒
          // ignore empty else blocks
        case _ ⇒
          withKeywordScope(owner, t, a.Else) { sElse ⇒
            body(sElse, elsep)
          }
      }
    case Match(selector, cases) ⇒
      body(owner, selector)
      withKeywordScope(owner, t, a.Match) { sMatch ⇒
        cases foreach (body(sMatch, _))
      }
    case CaseDef(pat, guard, body) ⇒
      withKeywordScope(owner, t, a.Case) { sCase ⇒
        this.body(sCase, pat)
        this.body(sCase, guard)
        this.body(sCase, body)
      }
    case TTry(block, catchCases, finalizer) ⇒
      withKeywordScope(owner, t, a.Try) { sTry ⇒
        body(sTry, block)
      }
      if (catchCases.nonEmpty) {
        withKeywordScope(owner, t, a.Catch) { sCatch ⇒
          catchCases foreach (body(sCatch, _))
        }
      }
      if (finalizer.nonEmpty) {
        withKeywordScope(owner, t, a.Finally) { sFinally ⇒
          body(sFinally, finalizer)
        }
      }
    case Throw(expr) ⇒
      body(owner, expr)
    case Return(expr) ⇒
      body(owner, expr)
    case _: This ⇒
      refTree(owner, t)
    case LabelDef(_, _, If(cond, Block(stats, _), _)) ⇒
      withKeywordScope(owner, t, a.While) { sWhile ⇒
        body(sWhile, cond)
        stats foreach (body(sWhile, _))
      }
    case LabelDef(_, _, Block(stats, If(cond, _, _))) ⇒
      withKeywordScope(owner, t, a.Do) { sDo ⇒
        body(sDo, cond)
        stats foreach (body(sDo, _))
      }
    case EmptyTree ⇒
    case _: Apply ⇒
      refTree(owner, t)
    case _: TypeApply ⇒
      refTree(owner, t)
    case _: TypeTree ⇒
      typeRef(owner, t)
    case Function(vparams, body) ⇒
      withNewScope {
        vparams foreach (valDef(owner, _, codeOrder, isFunction = true))
        this.body(owner, body)
      }
    case Bind(_, body) ⇒
      val decl = mkDeclWithPos(t.symbol, owner, t.pos)
      found += decl
      scopes = scopes.add(decl)
      this.body(owner, body)
    case Typed(expr, tpt) ⇒
      body(owner, expr)
      typeRef(owner, tpt)
    case UnApply(fun, args) ⇒
      body(owner, fun)
      args foreach (body(owner, _))
    case t: Ident ⇒
      if (t.name != nme.USCOREkw)
        refTree(owner, t)
    case Alternative(trees) ⇒
      trees foreach (body(owner, _))
    case t ⇒
      throwTreeMatchError(t)
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

  private def anonClassDef(owner: h.Hierarchy, t: ClassDef) = t match {
    case ClassDef(_, _, _, Template(_, _, body)) ⇒
      body foreach {
        // partial function applications like `f { case x ⇒ }` are transformed
        // in a way that the partial function is put into the body of the method
        // `applyOrElse` of an anonymous class.
        case DefDef(_, TermName("applyOrElse"), _, _, _, Match(_, cases)) ⇒
          cases filter (!_.pos.isTransparent) foreach (this.body(owner, _))
        case _ ⇒
      }
    case _ ⇒
  }

  private def classDef(owner: h.Hierarchy, t: ClassDef): Unit = {
    if (t.symbol.isAnonOrRefinementClass)
      anonClassDef(owner, t)
    else {
      annotationRef(owner, t.symbol, t.pos)
      val decl = mkDeclWithPos(t.symbol, owner, t.pos)
      found += decl
      withNewScope {
        t.tparams foreach (typeParamDef(decl, _))
        template(decl, t.impl)
      }
    }
  }

  private def moduleDef(owner: h.Hierarchy, t: ModuleDef): Unit = {
    annotationRef(owner, t.symbol, t.pos)
    val decl = mkDeclWithPos(t.symbol, owner, t.pos)
    found += decl
    withNewScope {
      template(decl, t.impl)
    }
  }

  private def importDef(owner: h.Hierarchy, t: Import): Unit = {
    def ref(sym: Symbol, name: Name, pos: Int): h.Ref = {
      val decl = mkDeepDecl(sym)
      // symbols which point to package imports do not have a source file associated
      // with them, therefore we have to set a position manually in order to ensure
      // that a SourceFile attachment is correctly set
      if (sym.hasPackageFlag) {
        decl.position = h.RangePosition(pos, pos + name.length)
        classifyDecl(sym, decl)
      }
      // we have to use `name` here instead of `decl.name` because the latter
      // points to the declaration but `name` may be a renamed selector and
      // therefore its name can differ.
      val ref = mkRef(t, name.toString, decl, owner, decl.owner)
      ref.position = h.RangePosition(pos, pos+ref.name.length)
      ref
    }

    val Import(qualifier, selectors) = t
    refTree(owner, qualifier)
    selectors foreach { sel ⇒
      if (sel.name != nme.WILDCARD) {
        // The selector has no symbol attached, therefore we have to find the
        // type or term by searching for it with the `member` method.
        val typeSym = qualifier.tpe.member(sel.name.toTypeName)
        val sym =
          if (typeSym.exists)
            typeSym
          else
            qualifier.tpe.member(sel.name.toTermName)
        found += ref(sym, sel.name, sel.namePos)

        if (sel.name != sel.rename)
          found += ref(sym, sel.rename, sel.renamePos)
      }
    }
  }

  private def typeDef(owner: h.Hierarchy, t: TypeDef): Unit = {
    val decl = mkDeclWithPos(t.symbol, owner, t.pos)
    found += decl
    t.tparams foreach (typeParamDef(decl, _))
    body(owner, t.rhs)
  }

  private def valDef(owner: h.Hierarchy, t: ValDef, codeOrder: Int, isFunction: Boolean = false): Unit = {
    annotationRef(owner, t.symbol, t.pos)
    if (t.symbol.isSynthetic || t.symbol.isLazy)
      return
    val decl = mkDeclWithPos(t.symbol, owner, t.pos)
    if (isFunction)
      decl.addAttachments(a.Function)
    // implicit flag is only added to the getter
    if (t.symbol.hasGetter) {
      val g = t.symbol.getterIn(t.symbol.owner)
      if (g.isAccessor && g.isImplicit)
        decl.addAttachments(a.Implicit)
    }
    decl.addAttachments(a.CodeOrder(codeOrder))
    found += decl
    scopes = scopes.add(decl)
    withNewScope {
      typeRef(decl, t.tpt)
      body(decl, t.rhs)
    }
  }

  private def selfRef(owner: h.Hierarchy, t: ValDef): Unit = {
    if (t == noSelfType)
      return
    val decl = mkDecl(t.symbol, owner)
    // the end position for self references are wrong, we have to set the end manually
    decl.position = h.RangePosition(t.pos.start, t.pos.start+decl.name.length)
    found += decl
    typeRef(decl, t.tpt, selfRefPos = Some(t.pos.start))
  }

  private def defDef(owner: h.Hierarchy, t: DefDef): Unit = {
    val DefDef(_, _, tparams, vparamss, tpt, rhs) = t

    def normalDefDef() = {
      annotationRef(owner, t.symbol, t.pos)
      val decl = mkDecl(t.symbol, owner)
      if (t.name == nme.CONSTRUCTOR) {
        decl.addAttachments(a.Constructor)
        if (t.pos.isTransparent || t.pos.isOffset)
          setPositionOfOwner(owner, decl)
        // we need to catch auxiliary constructors here (they have a range position)
        // because the implementation of `setPosition` for some reason can't handle them.
        else
          decl.position = h.RangePosition(t.pos.point, t.pos.point+"this".length)
      }
      else
        setPosition(decl, t.pos)
      found += decl
      withNewScope {
        tparams foreach (typeParamDef(decl, _))
        vparamss foreach (_ foreach (valDef(decl, _)))
        val isGeneratedSetter = vparamss.headOption.flatMap(_.headOption).exists(_.symbol.isSetterParameter)
        if (!isGeneratedSetter && t.name != nme.CONSTRUCTOR)
          typeRef(decl, tpt)
        // do not index bodies of generated code
        if (!t.symbol.isSynthetic) {
          // not sure if this condition is the right thing to do. It avoids to create
          // refs to the default constructor `java.lang.Object.<ref>this()V`. The
          // default constructor of the super class is always called implicitly but
          // I'm not sure if we want to highlight this fact in our index.
          if (!(t.name == nme.CONSTRUCTOR && (t.pos.isOffset || t.pos.isTransparent)))
            body(decl, rhs)
        }
      }
    }

    def lazyDefDef() = {
      found += mkDeclWithPos(t.symbol, owner, t.pos)
      typeRef(owner, tpt)
      body(owner, rhs)
    }

    if (t.symbol.isLazy)
      lazyDefDef
    else if (t.name == nme.MIXIN_CONSTRUCTOR
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
    val decl = mkDeclWithPos(t.symbol, owner, t.pos)
    found += decl
    t.tparams foreach (typeParamDef(decl, _))
  }

  private def packageDef(t: Tree): h.Decl = t match {
    case Select(qualifier, name) ⇒
      mkDeclWithPos(t.symbol, packageDef(qualifier), t.pos)
    case Ident(nme.EMPTY_PACKAGE_NAME) ⇒
      h.Root
    case _: Ident ⇒
      mkDeclWithPos(t.symbol, h.Root, t.pos)
    case t ⇒
      throwTreeMatchError(t)
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
    case t ⇒
      throwTreeMatchError(t)
  }

  private def setPosition(d: h.HierarchyWithName, pos: Position, skipping: Movement = Movements.none) = {
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

  private def withNewScope(f: ⇒ Unit) = {
    scopes = scopes.inc
    f
    scopes = scopes.dec
  }

  private def withKeywordScope(owner: h.Hierarchy, t: Tree, attachment: a)(f: h.Scope ⇒ Unit) = {
    val s = h.Scope(owner)
    s.position = h.RangePosition(t.pos.start, t.pos.start+attachment.asString.length)
    s.addAttachments(attachment)
    found += s
    withNewScope(f(s))
  }

  private def throwTreeMatchError[A](t: Tree) = {
    val lineNumber = t.pos.source.offsetToLine(t.pos.start)
    val line = t.pos.source.lineToString(lineNumber)
    val line2 = if (lineNumber > 0) Seq(t.pos.source.lineToString(lineNumber - 1)) else Nil
    val line3 = if (lineNumber > 1) Seq(t.pos.source.lineToString(lineNumber - 2)) else Nil
    val lineStart = t.pos.source.lineToOffset(lineNumber)
    val column = t.pos.start - lineStart
    val marker = " " * column + "^"
    val matchErr = new MatchError(t).getMessage().split("\n")
    throw new IllegalStateException(s"Match error at tree conversion (line ${lineNumber + 1}, column ${column + 1}):\n" +
        List(line3, line2, Seq(line), Seq(marker), matchErr.toSeq).flatten.map("  " + _).mkString("\n"))
  }
}

final case class Scopes(level: Int = 0, scopes: Map[Int, Map[String, h.Decl]] = Map(0 → Map())) {
  def asDecl(str: String): Option[h.Decl] = {
    def find(level: Int): Option[h.Decl] = {
      if (level < 0)
        None
      else
        scopes(level).get(str).orElse(find(level - 1))
    }
    find(level)
  }
  def inc: Scopes =
    copy(level = level + 1, scopes = scopes + ((level + 1) → Map()))
  def dec: Scopes =
    copy(level = level - 1, scopes = scopes - level)
  def add(decl: h.Decl): Scopes = {
    val ident = {
      val d = h.Decl(decl.name, h.Root)
      d.addAttachments(decl.attachments.toSeq: _*)
      d.asString
    }
    require(!scopes(level).contains(ident), s"Value `$ident` already exists.")
    copy(scopes = scopes + (level → (scopes(level) + (ident → decl))))
  }
}
