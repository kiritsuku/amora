package backend

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

import research.converter.ClassfileConverterTest
import research.converter.ScalacConverterTest
import backend.indexer.JavaBytecodeIndexerTest
import backend.indexer.DeprecatedScalaSourceIndexerTest
import backend.indexer.IndexerTest
import backend.indexer.ScalaSourceIndexerTest
import backend.indexer.ScalaSourceRegionIndexerTest

@RunWith(classOf[Suite])
@SuiteClasses(Array(
  classOf[ScalacConverterTest],
  classOf[DeprecatedScalaSourceIndexerTest],
  classOf[ClassfileConverterTest],
  classOf[JavaBytecodeIndexerTest],
  classOf[IndexerTest],
  classOf[ScalaSourceIndexerTest],
  classOf[ScalaSourceRegionIndexerTest]
))
class TestSuite
