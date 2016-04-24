package research

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

import research.converter.ClassfileConverterTest
import research.converter.ScalacConverterTest
import research.indexer.ScalaSourceIndexerTest
import research.indexer.RegionIndexerTest

@RunWith(classOf[Suite])
@SuiteClasses(Array(
  classOf[ScalacConverterTest],
  classOf[ScalaSourceIndexerTest],
  classOf[RegionIndexerTest],
  classOf[ClassfileConverterTest]
))
class TestSuite
