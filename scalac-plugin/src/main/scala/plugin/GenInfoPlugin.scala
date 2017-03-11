package plugin

import java.io.PrintWriter
import java.io.StringWriter
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.util.Failure
import scala.util.Success

import amora.backend.schema.Schema
import amora.converter.ScalacConverter
import amora.converter.protocol._

class GenInfoPlugin(override val global: Global) extends Plugin {
  override val name = "GenInfoPlugin"

  override val description = "Generates information from scalac trees"

  override val components = List(new GenInfoComponent(global))
}

class GenInfoComponent(override val global: Global) extends PluginComponent {
  import global._

  override def newPhase(prev: Phase): Phase = new Phase(prev) {
    override def run(): Unit = {
      if (currentRun.units.isEmpty)
        return

      val outputDir = global.settings.outputDirs.getSingleOutput.getOrElse {
        throw new IllegalStateException("scalac has no output directory configured, therefore Amora compiler plugin can't store its data.")
      }.file.getAbsolutePath

      println("Amora compiler plugin writes databases:")
      currentRun.units foreach { u ⇒
        import scala.collection.JavaConverters._
        val file = u.source.file.file
        val fileName = file.getAbsolutePath.replace('/', '%')
        val filePath = s"$outputDir/$fileName.amoradb"
        val hierarchy = new ScalacConverter[global.type](global).convert(u.body)

        val data = hierarchy match {
          case Success(res) ⇒
            Schema.mkTurtleUpdate(File(Artifact(Project("p"), "o", "n", "v1"), file.getName), res)
          case Failure(f) ⇒
            val sw = new StringWriter
            val pw = new PrintWriter(sw)
            f.printStackTrace(pw)
            sw.toString()
        }

        Files.write(Paths.get(filePath), Seq(data).asJava, Charset.forName("UTF-8"))
        if (hierarchy.isSuccess)
          println(s"- success:     $filePath")
        else
          println(s"- with errors: $filePath")
      }
    }
    override def name = "GenInfoPhase"
  }

  override val phaseName = "GenInfoComponent"

  override val runsAfter = List("typer")
}
