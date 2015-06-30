// First install all npm modules with "npm update"
// Then run the app with "npm start"

enablePlugins(ScalaJSPlugin, SbtWeb)

name := "Scala.js Tutorial"
scalaVersion := "2.11.6"
scalaJSStage in Global := FastOptStage

EclipseKeys.withSource := true

/* The following installs were required:
 * - In pacman:
 *   - nodejs
 *   - phantomjs
 * - Manual:
 *   - https://github.com/nwjs/nw.js
 */
resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases")

libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0"
// https://github.com/antonkulaga/codemirror-facade
libraryDependencies += "org.denigma" %%% "codemirror-facade" % "5.3-0.5"
libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
// suppress warning about multiple versions found
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
skip in packageJSDependencies := false
jsDependencies += RuntimeDOM
testFrameworks += new TestFramework("utest.runner.Framework")

persistLauncher in Compile := true
persistLauncher in Test := false
