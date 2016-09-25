package amora.converter

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts._

object DotcConverterApp extends App {

  implicit val ctx = (new ContextBase).initialCtx.fresh
  val compiler = new Compiler {
    override def phases = {
      val ps = super.phases
      // Right now there are 3 phases before typer
      // One can look up how many phases there are in the super implementation
      val beforeTyper = ps.take(3)
      beforeTyper :+ List(new AmoraPhase)
    }
  }
  val run = compiler.newRun
  run.compile("""
    class X
  """)
  run.units foreach { u ⇒
    val t = u.tpdTree
    println(t)
  }
}
