package amora.backend.services

import java.io.PrintWriter
import java.io.StringWriter

import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter
import scala.util.Failure
import scala.util.Success

import amora.backend.Logger
import amora.backend.schema.Schema
import amora.converter.ScalacConverter
import amora.converter.protocol._

/**
 * Provides functionality to extract information out of Scala source code.
 */
final class ScalaSourceIndexer(logger: Logger) extends ScalaService {

  def run(origin: String, data: Seq[(String, String)]): String = {
    handleScalaSource(origin, data)

    response(s"""
      @prefix service:<http://amora.center/kb/Schema/Service/> .
      @prefix response:<http://amora.center/kb/ServiceResponse/> .
      <#this>
        a response: ;
        service:requestId <$requestId> ;
      .
    """)
  }

  private def handleScalaSource(origin: String, data: Seq[(String, String)]): Unit = {
    import amora.api._

    val model = turtleModel(origin)
    val res = convertToHierarchy(data)
    val artifact = sparqlQuery"""
      prefix Artifact:<http://amora.center/kb/amora/Schema/Artifact/>
      prefix Project:<http://amora.center/kb/amora/Schema/Project/>
      select * where {
        [
          Artifact:organization ?organization ;
          Artifact:name ?name ;
          Artifact:version ?version ;
          Artifact:owner [Project:name ?pname] ;
        ] a Artifact: .
      }
    """.runOnModel(model).map { row ⇒
      Artifact(
          Project(row.string("pname")),
          row.string("organization"),
          row.string("name"),
          row.string("version"))
    }.head
    def mkPkg(pkgs: Seq[String]): Schema = pkgs match {
      case Nil ⇒ artifact
      case pkg +: pkgs ⇒ Package(pkg, mkPkg(pkgs))
    }
    val PkgFinder = """(?s).*?package ([\w\.]+).*?""".r

    res foreach {
      case (fileName, hierarchy) ⇒
        logger.info(s"Indexing ${hierarchy.size} entries of file $fileName")

        val src = data.find(_._1 == fileName).get._2
        val file = src match {
          case PkgFinder(name) ⇒ File(mkPkg(name.split('.').reverse), fileName)
          case _ ⇒ File(artifact, fileName)
        }
        val s = Schema.mkTurtleUpdate(file, hierarchy)
        turtleUpdate(origin, s"Error happened while indexing $fileName.")
        turtleUpdate(s, s"Error happened while indexing $fileName.")
    }
  }

  /**
   * Converts pairs of file names and Scala sources to pairs of file names and
   * hierarchies.
   *
   * File names that end with ".java" won't be converted to a hierarchy but they
   * can be used as dependencies for the Scala files.
   */
  def convertToHierarchy(data: Seq[(String, String)]): Seq[(String, Seq[Hierarchy])] = {
    def srcOf[A : reflect.ClassTag] = reflect.classTag[A].runtimeClass.getProtectionDomain.getCodeSource
    val stdlibSrc = srcOf[Predef.type]

    val s = new Settings
    // - When the tests are run in Eclipse stdlibSrc is null. Luckily, we don't need
    //   to add the stdlib to the classpath in Eclipse.
    // - When the tests are run in sbt stdlibSrc is not null and we have to add it
    //   to the classpath because scalac can't find it otherwise.
    //   It doesn't even work in sbt when `fork in Test := true` is set.
    if (stdlibSrc != null)
      s.bootclasspath.value = stdlibSrc.getLocation.toExternalForm()

    val writer = new StringWriter
    val reporter = new ConsoleReporter(s, Console.in, new PrintWriter(writer, /* autoFlush */true))
    val g = new Global(s, reporter)

    logger.info(s.userSetSettings.toSeq.sortBy(_.name.toLowerCase).mkString(s"Compile sources with scalac settings:\n  ", "\n  ", ""))

    def withResponse[A](f: g.Response[A] ⇒ Unit) = {
      val r = new g.Response[A]
      f(r)
      r
    }

    val sfs = data map {
      case (filename, src) ⇒
        val sf = g.newSourceFile(src, filename)
        filename → sf
    }
    withResponse[Unit] { g.askReload(sfs.map(_._2).toList, _) }.get

    val res = sfs.filter(!_._1.endsWith(".java")) map {
      case (filename, sf) ⇒
        val tree = withResponse[g.Tree](g.askLoadedTyped(sf, keepLoaded = true, _)).get.left.get

        if (reporter.hasErrors || reporter.hasWarnings)
          throw new IllegalStateException(s"Errors occurred during compilation of file `$filename`:\n$writer")

        g ask { () ⇒ new ScalacConverter[g.type](g).convert(tree) } match {
          case Success(res) ⇒
            filename → res
          case Failure(f) ⇒
            throw f
        }
    }
    g.askShutdown()
    res
  }
}
