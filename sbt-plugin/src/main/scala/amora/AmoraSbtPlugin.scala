import sbt._, Keys._
import sbt.plugins.JvmPlugin

object AmoraSbtPlugin extends AutoPlugin {

  override def requires = JvmPlugin
  override def trigger = AllRequirements
  override def projectSettings = scalacPluginSettings

  lazy val scalacPluginSettings = Def.settings(
    libraryDependencies ++= (scalaVersion.value match {
      case "2.11.8" =>
        List("amora" %% "scalac-plugin" % "0.1-SNAPSHOT" cross CrossVersion.full)
      case _ =>
        Nil
    }),
    scalacOptions ++= (scalaVersion.value match {
      case "2.11.8" =>
        val jarRegex = """scalac-plugin_2.11.8.jar"""
        val allFiles = update.value.allFiles
        val amoraJar = allFiles.collectFirst {
          case file if file.getName matches jarRegex => file.getAbsolutePath
        }.getOrElse {
          throw new IllegalStateException(s"Unable to find Amora compiler plugin JAR. All files: $allFiles")
        }

        sLog.value.info(s"Loading Amora compiler plugin: $amoraJar")

        List(s"-Xplugin:$amoraJar")
      case _ =>
        Nil
    })
  )
}
