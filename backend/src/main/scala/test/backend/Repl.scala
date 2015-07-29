package test.backend

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Response
import scala.tools.nsc.settings.ScalaVersion
import scala.reflect.internal.util.AbstractFileClassLoader
import scala.tools.nsc.interpreter.ILoop
import java.io.PrintStream
import java.io.ByteArrayOutputStream

class Repl {
  val outputDir = new VirtualDirectory("<virtualdir>", None)
  val s = new Settings(err ⇒ Console.err.println(err))
  s.outputDirs.setSingleOutput(outputDir)
  s.usejavacp.value = true
  s.source.value = ScalaVersion("2.11.7")

  val loop = new ILoop()
  loop.settings = s
  loop.createInterpreter()

  def interpret(expr: String): Unit = {
    loop.intp.interpret(expr)
    /*
    val m = classOf[ILoop].getMethod("compile", classOf[String], classOf[Boolean])
    m.setAccessible(true)
    val synthetic = java.lang.Boolean.valueOf(false)
    val res = m.invoke(loop, expr, synthetic)
    import scala.tools.nsc.interpreter._
    val intp = loop.intp
    import intp._
    res.asInstanceOf[Either[IR.Result, Request]] match {
      case Left(result) => result
      case Right(req)   =>
        new WrappedRequest(req).loadAndRunReq
    }
    *
    */
  }

  /*
  val reporter = new StoreReporter
  val compiler = new Global(s, reporter)
  val classLoader = new AbstractFileClassLoader(outputDir, getClass.getClassLoader)

  def compile(expr: String, className: String): Unit = {
    val src = s"""
      class $className {
        $expr
      }
    """
    val srcFile = new BatchSourceFile("<memory>", src)
    val run = new compiler.Run
    compiler ask { () ⇒ run.compileSources(List(srcFile)) }

    if (reporter.hasErrors || reporter.hasWarnings)
      throw new IllegalStateException(reporter.infos.mkString("Errors occurred during compilation of extension wrapper:\n", "\n", ""))
  }

  def execute(expr: String) = {
    val className = "Box" + System.nanoTime
    compile(expr, className)

    val cls = classLoader.tryToInitializeClass(className)
  }
  */
}
