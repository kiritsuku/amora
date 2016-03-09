package plugin

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent

class GenInfoPlugin(override val global: Global) extends Plugin {
  override val name = "GenInfoPlugin"

  override val description = "Generates information from scalac trees"

  override val components = List(new GenInfoComponent(global))
}

class GenInfoComponent(override val global: Global) extends PluginComponent {

  import global._

  override def newPhase(prev: Phase): Phase = new Phase(prev) {
    override def run() = {
      val u = currentRun.units.toList

      def idents(t: Tree) = {
        val trav = new IdentFinder
        trav.traverse(t)
        trav.idents.toList
      }

      println(u.map(_.body) map idents)
    }
    override def name = "GenInfoPhase"
  }

  override val phaseName = "GenInfoComponent"

  override val runsAfter = List("typer")

  class IdentFinder extends Traverser {
    val idents = ListBuffer[String]()

    override def traverse(tree: Tree) = {
      tree match {
        case PackageDef(pid, stats)                          ⇒
        case ClassDef(mods, name, tparams, impl)             ⇒
        case ModuleDef(mods, name, impl)                     ⇒
        case ValDef(mods, name, tpt, rhs)                    ⇒
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) ⇒
        case TypeDef(mods, name, tparams, rhs)               ⇒
        case LabelDef(name, params, rhs)                     ⇒
        case Import(expr, selectors)                         ⇒
        case DocDef(comment, definition)                     ⇒
        case Template(parents, self, body)                   ⇒
        case Block(stats, expr)                              ⇒
        case CaseDef(pat, guard, body)                       ⇒
        case Alternative(trees)                              ⇒
        case Star(elem)                                      ⇒
        case Bind(name, body)                                ⇒
        case UnApply(fun, args)                              ⇒
        case ArrayValue(elemtpt, trees)                      ⇒
        case Function(vparams, body)                         ⇒
        case Assign(lhs, rhs)                                ⇒
        case AssignOrNamedArg(lhs, rhs)                      ⇒
        case If(cond, thenp, elsep)                          ⇒
        case Match(selector, cases)                          ⇒
        case Return(expr)                                    ⇒
        case Try(block, catches, finalizer)                  ⇒
        case Throw(expr)                                     ⇒
        case New(tpt)                                        ⇒
        case Typed(expr, tpt)                                ⇒
        case TypeApply(fun, args)                            ⇒
        case Apply(fun, args)                                ⇒
        case ApplyDynamic(qual, args)                        ⇒
        case Super(qual, mix)                                ⇒
        case This(qual)                                      ⇒
        case Select(qualifier, selector)                     ⇒
        case Ident(name)                                     ⇒
        case Literal(value)                                  ⇒
        case TypeTree()                                      ⇒
        case Annotated(annot, arg)                           ⇒
        case SingletonTypeTree(ref)                          ⇒
        case SelectFromTypeTree(qualifier, selector)         ⇒
        case CompoundTypeTree(templ)                         ⇒
        case AppliedTypeTree(tpt, args)                      ⇒
        case TypeBoundsTree(lo, hi)                          ⇒
        case ExistentialTypeTree(tpt, whereClauses)          ⇒
        case SelectFromArray(qualifier, selector, erasure)   ⇒
        case EmptyTree                                       ⇒
      }
      super.traverse(tree)
    }
  }

}
