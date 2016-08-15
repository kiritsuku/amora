package backend

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

import research.converter.ClassfileConverterTest
import research.converter.ScalacConverterTest
import backend.indexer.JavaBytecodeIndexerTest
import backend.indexer.ScalaSourceIndexerTest
import backend.indexer.RegionIndexerTest
import backend.indexer.IndexerTest

@RunWith(classOf[Suite])
@SuiteClasses(Array(
  classOf[ScalacConverterTest],
  classOf[ScalaSourceIndexerTest],
  classOf[RegionIndexerTest],
  classOf[ClassfileConverterTest],
  classOf[JavaBytecodeIndexerTest],
  classOf[IndexerTest]
))
class TestSuite
