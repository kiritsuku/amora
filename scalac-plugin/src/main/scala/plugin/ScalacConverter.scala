package plugin

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.Global

class ScalacConverter[G <: Global](val global: G) {
  import global._

  def findIdents(tree: Tree): Set[String] = {
    val t = new IdentFinder
    t.traverse(tree)
    t.idents.toSet
  }

  private class IdentFinder extends Traverser {
    val idents = ListBuffer[String]()

    def fullName(t: Tree) =
      t.symbol.ownerChain.reverse.tail.map(_.nameString).mkString(".")

    override def traverse(tree: Tree) = {
      tree match {
        case PackageDef(pid, stats) ⇒
          idents += fullName(tree)
        case ClassDef(mods, name, tparams, impl) ⇒
          idents += fullName(tree)
        case ModuleDef(mods, name, impl) ⇒
          idents += fullName(tree)
        case ValDef(mods, name, tpt, rhs) ⇒
          val retName = tpt.tpe.typeSymbol.fullNameString
          idents += retName
          idents += fullName(tree)
        case t: DefDef if t.name == nme.CONSTRUCTOR ⇒
          // skip
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) ⇒
          val argsNames = tpt.tpe.typeArguments.map(_.typeSymbol.fullNameString)
          idents ++= argsNames
          val retName = tpt.tpe.typeSymbol.fullNameString
          idents += retName
          idents += fullName(tree)
        case TypeDef(mods, name, tparams, rhs) ⇒
          idents += fullName(tree)
        case LabelDef(name, params, rhs) ⇒
          idents += fullName(tree)
        case Import(expr, selectors)       ⇒
        case DocDef(comment, definition)   ⇒
        case Template(parents, self, body) ⇒
        case Block(stats, expr)            ⇒
        case CaseDef(pat, guard, body)     ⇒
        case Alternative(trees)            ⇒
        case Star(elem)                    ⇒
        case Bind(name, body) ⇒
          idents += fullName(tree)
        case UnApply(fun, args)             ⇒
        case ArrayValue(elemtpt, trees)     ⇒
        case Function(vparams, body)        ⇒
        case Assign(lhs, rhs)               ⇒
        case AssignOrNamedArg(lhs, rhs)     ⇒
        case If(cond, thenp, elsep)         ⇒
        case Match(selector, cases)         ⇒
        case Return(expr)                   ⇒
        case Try(block, catches, finalizer) ⇒
        case Throw(expr)                    ⇒
        case New(tpt)                       ⇒
        case Typed(expr, tpt)               ⇒
        case TypeApply(fun, args)           ⇒
        case Apply(fun, args)               ⇒
        case ApplyDynamic(qual, args)       ⇒
        case Super(qual, mix)               ⇒
        case This(qual)                     ⇒
        case Select(qualifier, selector)    ⇒
        case Ident(name) ⇒
          idents += fullName(tree)
        case Literal(value)                                ⇒
        case TypeTree()                                    ⇒
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
      super.traverse(tree)
    }
  }

}
