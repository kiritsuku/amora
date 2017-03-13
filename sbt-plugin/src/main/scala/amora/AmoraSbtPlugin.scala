import sbt._, Keys._
import sbt.plugins.JvmPlugin
import scalaz._
import scalaz.concurrent.Task

object AmoraSbtPlugin extends AutoPlugin {

  override def requires = JvmPlugin
  override def trigger = AllRequirements
  override def projectSettings = scalacPluginSettings

  lazy val scalacPluginSettings = Def.settings(
    scalacOptions ++= (scalaVersion.value match {
      case "2.11.8" =>
        val amoraJar = resolveSingleJar(scalaVersion.value, "amora" %% "scalac-plugin" % "0.1-SNAPSHOT" cross CrossVersion.full)
        sLog.value.info(s"Loading Amora compiler plugin: $amoraJar")

        List(s"-Xplugin:$amoraJar")
      case _ =>
        Nil
    })
  )

  private val repositories = List(
    coursier.MavenRepository(DefaultMavenRepository.root),
    coursier.MavenRepository(Resolver.sonatypeRepo("releases").root),
    coursier.Cache.ivy2Local)

  private def resolveSingleJar(scalaVersion: String, module: ModuleID): File = {
    val mod = CrossVersion(scalaVersion, CrossVersion.binaryScalaVersion(scalaVersion))(module.intransitive)
    resolve(mod).head.getCanonicalFile
  }

  private def resolve(modules: ModuleID*): Seq[File] = {
    val resolution = coursier.Resolution(modules.map { module =>
      coursier.Dependency(
        coursier.Module(module.organization, module.name),
        module.revision,
        configuration = module.configurations.getOrElse(""),
        transitive = module.isTransitive
      )
    }.toSet)

    val fetch = coursier.Fetch.from(repositories, coursier.Cache.fetch())
    val resolved = resolution.process.run(fetch).unsafePerformSync
    resolved.errors.foreach { err =>
      throw new RuntimeException(s"Failed to resolve $err")
    }

    Task.gatherUnordered(
      resolved.artifacts.map(coursier.Cache.file(_).run)
    ).unsafePerformSync.flatMap {
        case -\/(err)                                    => throw new RuntimeException(err.message)
        case \/-(file) if !file.getName.endsWith(".jar") => None
        case \/-(file)                                   => Some(file)
      }
  }
}
