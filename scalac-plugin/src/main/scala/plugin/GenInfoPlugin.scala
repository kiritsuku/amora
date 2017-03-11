package plugin

import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.util.Failure
import scala.util.Success

import amora.converter.ScalacConverter

class GenInfoPlugin(override val global: Global) extends Plugin {
  override val name = "GenInfoPlugin"

  override val description = "Generates information from scalac trees"

  override val components = List(new GenInfoComponent(global))
}

class GenInfoComponent(override val global: Global) extends PluginComponent {
  import global._

  override def newPhase(prev: Phase): Phase = new Phase(prev) {
    override def run() = {
      def mkHierarchy(t: global.Tree) = {
        new ScalacConverter[global.type](global).convert(t) match {
          case Success(res) ⇒
            res
          case Failure(f) ⇒
            f.printStackTrace()
            Nil
        }
      }
      val outputDir = global.settings.outputDirs.getSingleOutput.getOrElse {
        throw new IllegalStateException("scalac has no output directory configured, therefore Amora compiler plugin can't store its data.")
      }.file.getAbsolutePath

      val files = currentRun.units map { u ⇒
        val is = mkHierarchy(u.body)
        val filePath = u.source.file.file.getAbsolutePath
        val fileName = s"$outputDir/${filePath.replace('/', '%')}.amoradb"
        val data = is.map(_.asString).sorted

        import scala.collection.JavaConverters._
        Files.write(Paths.get(fileName), data.asJava, Charset.forName("UTF-8"))
        fileName
      }
      println("Amora compiler plugin wrote files:\n" + files.mkString("- ", "\n- ", ""))
    }
    override def name = "GenInfoPhase"
  }

  override val phaseName = "GenInfoComponent"

  override val runsAfter = List("typer")
}
