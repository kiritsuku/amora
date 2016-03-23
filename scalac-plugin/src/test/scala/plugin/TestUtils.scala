package plugin

import java.io.PrintWriter
import java.io.StringWriter

import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter
import scala.util.Failure
import scala.util.Success

import org.junit.ComparisonFailure

import indexer.hierarchy.Hierarchy
import util.LoggerConfig

object TestUtils extends AnyRef with LoggerConfig {

  var debugTests: Boolean = false

  final implicit class Assert_===[A](private val actual: A) extends AnyVal {
    def ===(expected: A): Unit = {
      if (actual != expected) {
        (actual, expected) match {
          case (actual: Seq[_], expected: Seq[_]) ⇒
            val as = actual.map(_.toString).sorted.mkString("\n")
            val es = expected.map(_.toString).sorted.mkString("\n")
            throw new ComparisonFailure("", es, as)
          case (actual: Set[_], expected: Set[_]) ⇒
            val as = actual.toSeq.map(_.toString).sorted.mkString("\n")
            val es = expected.toSeq.map(_.toString).sorted.mkString("\n")
            throw new ComparisonFailure("", es, as)
          case _ ⇒
            throw new ComparisonFailure("", expected.toString, actual.toString)
        }
      }
    }
  }

  /** `data` is a sequence of `(filename, src)` */
  def convertToHierarchy(data: (String, String)*): Seq[(String, Seq[Hierarchy])] = {
    val s = new Settings
    val writer = new StringWriter
    val reporter = new ConsoleReporter(s, Console.in, new PrintWriter(writer, /* autoFlush */true))
    val g = new Global(s, reporter)

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
    sfs map {
      case (filename, sf) ⇒
        val tree = withResponse[g.Tree](g.askLoadedTyped(sf, keepLoaded = true, _)).get.left.get

        if (reporter.hasErrors || reporter.hasWarnings)
          throw new IllegalStateException(s"Errors occurred during compilation of file `$filename`:\n$writer")

        val res = g ask { () ⇒ new ScalacConverter[g.type](g).convert(tree) }

        res match {
          case Success(res) ⇒
            filename → res
          case Failure(f) ⇒
            throw f
        }
    }
  }

}
