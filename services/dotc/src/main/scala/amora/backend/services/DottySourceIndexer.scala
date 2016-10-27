package amora.backend.services

import java.io.BufferedWriter
import java.io.File
import java.io.OutputStreamWriter

import scala.io.Codec
import scala.reflect.io.VirtualFile
import scala.util.Failure
import scala.util.Success

import amora.backend.Logger
import amora.converter.AmoraPhase
import amora.converter.protocol._
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.config.PathResolver
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.util.SourceFile

class DottySourceIndexer(logger: Logger) extends ScalaService {

  def run(data: Seq[(String, String)]): String = {
    handleDottySource(Artifact(Project("testProject"), "o", "n", "v1"), data)

    response(s"""
      @prefix service:<http://amora.center/kb/Schema/0.1/Service/0.1/> .
      @prefix response:<http://amora.center/kb/ServiceResponse/0.1/> .
      <#this>
        a response: ;
        service:requestId <$requestId> ;
      .
    """)
  }

  private def handleDottySource(origin: Schema, data: Seq[(String, String)]): Unit = {
    ???
  }

  def convertToSchema(data: Seq[(String, String)]): Seq[(String, Seq[Hierarchy])] = {
    def srcOf[A : reflect.ClassTag] = reflect.classTag[A].runtimeClass.getProtectionDomain.getCodeSource.getLocation.toExternalForm
    var results = Vector[(String, Seq[Hierarchy])]()
    implicit val ctx = {
      val stdlibSrc = srcOf[Predef.type]
      val dottylibSrc = srcOf[dotty.DottyPredef.type]

      val ctx = new ContextBase().initialCtx.fresh
      ctx.setSetting(ctx.settings.classpath, Seq(stdlibSrc, dottylibSrc).mkString(File.pathSeparator))
      ctx
    }
    val compiler = new Compiler {
      override def phases = {
        val ps = super.phases
        // Right now there are 3 phases before typer
        // One can look up how many phases there are in the super implementation
        val beforeTyper = ps.take(3)
        beforeTyper :+ List(new AmoraPhase((unit, res) ⇒ {
          res match {
            case Success(res) ⇒
              results :+= unit.source.file.name → res
            case Failure(f) ⇒
              throw f
          }
        }))
      }
    }
    val sourceFiles = data map {
      case (fileName, sourceCode) ⇒
        val virtualFile = new VirtualFile(fileName)
        val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8"))
        writer.write(sourceCode)
        writer.close()
        new SourceFile(virtualFile, Codec.UTF8)
    }
    val run = compiler.newRun
    logger.info("Compile sources with the following dotc configuration: " + new PathResolver().Calculated)
    run.compileSources(sourceFiles.toList)
    results.toSeq
  }
}
