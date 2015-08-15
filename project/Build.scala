import sbt._
import sbt.Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import com.typesafe.sbt.web.SbtWeb
import com.typesafe.sbt.web.Import.WebKeys
import com.typesafe.sbteclipse.core.EclipsePlugin._
import spray.revolver.RevolverPlugin._

object Build extends sbt.Build {

  val genElectronMain = TaskKey[Unit]("gen-electron-main", "Generates Electron application's main file.")

  lazy val commonSettings = Seq(
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:_",
      "-unchecked",
      "-Xlint",
      "-Xfuture",
      //"-Xfatal-warnings",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused-import",
      "-Ywarn-unused"
    ),

    EclipseKeys.withSource := true,

    incOptions := incOptions.value.withNameHashing(true),
    updateOptions := updateOptions.value.withCachedResolution(true)
  )

  lazy val shared = crossProject crossType CrossType.Pure in file("shared") settings (
    name := "scalajs-test-shared",
    // We need to explicitly set this to the default Eclipse output folder, otherwise another one is created
    EclipseKeys.eclipseOutput := Some("bin/"),

    libraryDependencies ++= deps.shared.value
  ) settings (commonSettings: _*)

  lazy val sharedJvm = shared.jvm

  lazy val sharedJs = shared.js

  lazy val electron = project in file("electron") enablePlugins(ScalaJSPlugin) settings commonSettings ++ Seq(
    name := "scalajs-test-electron",
    scalaJSStage in Global := FullOptStage,

    persistLauncher in Compile := true,
    persistLauncher in Test := false,

    /*
     * We need to generate the Electron's main files "package.json" and "main.js".
     */
    genElectronMain := {
      import java.nio.charset.Charset
      // TODO we rely on the files written on disk but it would be better to be able to get the actual content directly from the tasks
      val launchCode = IO.read((packageScalaJSLauncher in Compile).value.data, Charset.forName("UTF-8"))
      val jsCode = IO.read((fastOptJS in Compile).value.data, Charset.forName("UTF-8"))

      val pkgJson = """
      {
        "name": "electron-demo",
        "version": "0.1",
        "main": "main.js",
        "repository": {
          "type": "git",
          "url": "https://github.com/sschaef/scalajs-test"
        },
        "license": "MIT"
      }
      """.stripMargin

      // hack to get require and __dirname to work in the main process
      // see https://gitter.im/scala-js/scala-js/archives/2015/04/25
      val mainJs = s"""
        var addGlobalProps = function(obj) {
          obj.require = require;
          obj.__dirname = __dirname;
        }

        if((typeof __ScalaJSEnv === "object") && typeof __ScalaJSEnv.global === "object") {
          addGlobalProps(__ScalaJSEnv.global);
        } else if(typeof  global === "object") {
          addGlobalProps(global);
        } else if(typeof __ScalaJSEnv === "object") {
          __ScalaJSEnv.global = {};
          addGlobalProps(__ScalaJSEnv.global);
        } else {
          var __ScalaJSEnv = { global: {} };
          addGlobalProps(__ScalaJSEnv.global)
        }
        $jsCode
        $launchCode
      """

      val dest = (classDirectory in Compile).value / ".."
      IO.write(dest / "package.json", pkgJson)
      IO.write(dest / "main.js", mainJs)
    }
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
  ) dependsOn (sharedJs)

  lazy val backend = project in file("backend") settings commonSettings ++ Revolver.settings ++ Seq(
    name := "scalajs-test-backend",

    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies ++= deps.backend.value,

    // add *fastopt.js file to resources
    resourceGenerators in Compile <+= (fastOptJS in Compile in ui).map(r => Seq(r.data)),
    // add *fullopt.js file to resources
//    (resourceGenerators in Compile) <+= (fullOptJS in Compile in ui).map(r => Seq(r.data)),
    // add *launcher.js file to resources
    resourceGenerators in Compile <+= (packageScalaJSLauncher in Compile in ui).map(r => Seq(r.data)),
    // add *jsdeps.js file to resources
    resourceGenerators in Compile <+= (packageJSDependencies in Compile in ui).map(Seq(_)),
    // depend on the genElectronMain task but don't add its generated resources since we don't need to route them at runtime
    resourceGenerators in Compile <+= (genElectronMain in Compile in electron).map(_ => Seq()),
    // add folder of webjars to resources
    unmanagedResourceDirectories in Compile += (WebKeys.webTarget in Compile in ui).value / "web-modules" / "main" / "webjars" / "lib",

    // once the server is started, we also want to restart it on changes in the shared project
    watchSources ++= (watchSources in sharedJvm).value
  ) dependsOn (sharedJvm)

  object versions {
    val scalatags = "0.5.2"
  }

  object deps {
    lazy val shared = Def.setting(Seq(
      // https://github.com/ochrons/boopickle
      "me.chrons" %%% "boopickle" % "1.1.0"
    ))

    lazy val backend = Def.setting(Seq(
      "com.typesafe.akka" %% "akka-http-experimental" % "1.0",
      "com.typesafe.akka" %% "akka-stream-experimental" % "1.0",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scalameta" % "scalameta" % "0.1.0-SNAPSHOT" cross CrossVersion.binary,
      "org.scalameta" % "scalahost" % "0.1.0-SNAPSHOT" cross CrossVersion.full,
      "org.scalameta" %% "interpreter" % "0.1.0-SNAPSHOT",
      // https://github.com/ChrisNeveu/macrame
      "com.chrisneveu" %% "macrame" % "1.0.1",
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
      "com.lihaoyi" %%% "scalatags" % versions.scalatags
    ))

    lazy val sjsTest = Def.setting(Seq(
      "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
    ))

    lazy val sjs = Def.setting(Seq(
      "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
      // https://github.com/antonkulaga/codemirror-facade
      "org.denigma" %%% "codemirror-facade" % "5.5-0.5",
      // https://github.com/lihaoyi/scalatags
      "com.lihaoyi" %%% "scalatags" % versions.scalatags
    ))

    lazy val webjars = Def.setting(Seq(
      "org.webjars" % "codemirror" % "5.5" / "codemirror.js",
      // https://github.com/chjj/marked
      "org.webjars.bower" % "marked" % "0.3.3" / "marked.js",
      "org.webjars" % "d3js" % "3.5.5-1" / "d3.js",
      "org.webjars.bower" % "spin.js" % "2.3.1" / "spin.js"
    ))

    lazy val common = Def.setting(Seq(
      // use scalaVersion.value to suppress warning about multiple Scala versions found
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ))
  }
}
