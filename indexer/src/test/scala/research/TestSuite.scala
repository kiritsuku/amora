package research

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

import research.converter.ScalacConverterTest
import research.indexer.IndexerTest
import research.indexer.RegionIndexerTest

@RunWith(classOf[Suite])
@SuiteClasses(Array(
  classOf[ScalacConverterTest],
  classOf[IndexerTest],
  classOf[RegionIndexerTest]
))
class TestSuite
