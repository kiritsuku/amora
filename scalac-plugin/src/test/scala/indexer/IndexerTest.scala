package indexer

import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter

import org.junit.Test

import indexer.hierarchy.Hierarchy
import plugin.ScalacConverter
import plugin.TestUtils

class IndexerTest {

  import TestUtils._

  def convert(filename: String, src: String) = {
    val s = new Settings
    val r = new ConsoleReporter(s)
    val g = new Global(s, r)

    def withResponse[A](f: g.Response[A] ⇒ Unit) = {
      val r = new g.Response[A]
      f(r)
      r
    }

    val sf = g.newSourceFile(src, filename)
    val tree = withResponse[g.Tree](g.askLoadedTyped(sf, keepLoaded = true, _)).get.left.get
    val res = g ask { () ⇒ new ScalacConverter[g.type](g).convert(tree) }

    res match {
      case util.Success(res) ⇒
        res
      case util.Failure(f) ⇒
        throw f
    }
  }

  case class Data(varName: String, value: String)

  def ask(filename: String, data: Seq[Hierarchy], query: String)(implicit modelName: String): Seq[Data] = {
    Indexer.withInMemoryDataset { dataset ⇒
      Indexer.withModel(dataset, modelName) { model ⇒
        Indexer.add(modelName, filename, data)(model)
        val res = Indexer.queryResult(modelName, query, model) { (v, q) ⇒
          Data(v, q.get(v).toString)
        }
        return res.sortBy(d ⇒ (d.varName, d.value))
      }
    }
    throw new RuntimeException("An error happened during the test. See previous error message.")
  }

  @Test
  def find_top_level_classes() = {
    implicit val modelName = "http://test.model/"
    val filename = "<memory>"
    val data = convert(filename, """
      package a.b.c
      class X
      class Y
      class Z
    """)

    val result = ask(filename, data, s"""
      PREFIX c:<$modelName>
      SELECT * WHERE {
        ?class c:tpe "class" .
      }
    """)
    result === Seq(
        Data("class", s"${modelName}_root_/a/b/c/X"),
        Data("class", s"${modelName}_root_/a/b/c/Y"),
        Data("class", s"${modelName}_root_/a/b/c/Z")
    )
  }
}
