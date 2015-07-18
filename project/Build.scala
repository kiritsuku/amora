import sbt._
import sbt.Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import com.typesafe.sbt.web.SbtWeb
import com.typesafe.sbteclipse.core.EclipsePlugin._

object Build extends sbt.Build {

  lazy val commonSettings = Seq(
    scalaVersion := "2.11.7",

    EclipseKeys.withSource := true,

    incOptions := incOptions.value.withNameHashing(true),
    updateOptions := updateOptions.value.withCachedResolution(true)
  )

  lazy val ui = project in file("ui") enablePlugins(ScalaJSPlugin, SbtWeb) settings commonSettings ++ Seq(
    name := "scalajs-test-ui",
    scalaJSStage in Global := FastOptStage,

    resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases"),

    libraryDependencies ++= deps.sjs.value ++ deps.sjsTest.value ++ deps.common.value,

    skip in packageJSDependencies := false,
    jsDependencies += RuntimeDOM,
    jsDependencies ++= deps.webjars.value,
    testFrameworks += new TestFramework("utest.runner.Framework"),

    persistLauncher in Compile := true,
    persistLauncher in Test := false
  )

  lazy val backend = project in file("backend") settings commonSettings ++ Seq(
    name := "scalajs-test-backend"
  )

  object deps {
    lazy val sjsTest = Def.setting(Seq(
      "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
    ))

    lazy val sjs = Def.setting(Seq(
      "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
      // https://github.com/antonkulaga/codemirror-facade
      "org.denigma" %%% "codemirror-facade" % "5.3-0.5",
      // https://github.com/lihaoyi/scalatags
      "com.lihaoyi" %%% "scalatags" % "0.5.2"
    ))

    lazy val webjars = Def.setting(Seq(
      "org.webjars" % "codemirror" % "5.3" / "codemirror.js",
      // https://github.com/chjj/marked
      "org.webjars.bower" % "marked" % "0.3.3" / "marked.js"
    ))

    lazy val common = Def.setting(Seq(
      // use scalaVersion.value to suppress warning about multiple Scala versions found
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ))
  }
}
