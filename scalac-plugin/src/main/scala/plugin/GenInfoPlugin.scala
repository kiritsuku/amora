package plugin

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.util.Failure
import scala.util.Success

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

      def idents(t: global.Tree) = {
        new ScalacConverter[global.type](global).convert(t) match {
          case Success(res) ⇒
            res
          case Failure(f) ⇒
            f.printStackTrace()
            Nil
        }
      }

      println(u.map(_.body) map idents)
    }
    override def name = "GenInfoPhase"
  }

  override val phaseName = "GenInfoComponent"

  override val runsAfter = List("typer")
}
