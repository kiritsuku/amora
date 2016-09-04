package backend

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

import research.converter.ClassfileConverterTest
import research.converter.ScalacConverterTest
import backend.indexer.JavaBytecodeIndexerTest
import backend.indexer.IndexerTest
import backend.indexer.ScalaSourceIndexerTest
import backend.indexer.ScalaSourceRegionIndexerTest
import backend.services.FindUsagesTest

@RunWith(classOf[Suite])
@SuiteClasses(Array(
  classOf[ScalacConverterTest],
  classOf[ClassfileConverterTest],
  classOf[JavaBytecodeIndexerTest],
  classOf[IndexerTest],
  classOf[ScalaSourceIndexerTest],
  classOf[ScalaSourceRegionIndexerTest],
  classOf[FindUsagesTest]
))
class TestSuite
