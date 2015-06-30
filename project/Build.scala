import sbt._
import sbt.Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import com.typesafe.sbt.web.SbtWeb
import com.typesafe.sbteclipse.core.EclipsePlugin._

object Build extends sbt.Build {

  // First install all npm modules with "npm update"
  // Then run the app with "npm start"

  lazy val root = project in file(".") enablePlugins(ScalaJSPlugin, SbtWeb) settings (
    name := "Scala.js Tutorial",
    scalaVersion := "2.11.6",
    scalaJSStage in Global := FastOptStage,

    EclipseKeys.withSource := true,

    /* The following installs were required:
     * - In pacman:
     *   - nodejs
     *   - phantomjs
     * - Manual:
     *   - https://github.com/nwjs/nw.js
     */
    resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases"),

    libraryDependencies ++= deps.sjs.value ++ deps.sjsTest.value ++ deps.common.value,

    skip in packageJSDependencies := false,
    jsDependencies += RuntimeDOM,
    testFrameworks += new TestFramework("utest.runner.Framework"),

    persistLauncher in Compile := true,
    persistLauncher in Test := false
  )

  object deps {
    lazy val sjsTest = Def.setting(Seq(
      "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
    ))

    lazy val sjs = Def.setting(Seq(
      "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
      // https://github.com/antonkulaga/codemirror-facade
      "org.denigma" %%% "codemirror-facade" % "5.3-0.5"
    ))

    lazy val common = Def.setting(Seq(
      // use scalaVersion.value to suppress warning about multiple Scala versions found
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ))
  }
}
