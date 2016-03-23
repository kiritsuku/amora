package plugin

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

import indexer.IndexerTest
import indexer.RegionIndexerTest

@RunWith(classOf[Suite])
@SuiteClasses(Array(
  classOf[ScalacConverterTest],
  classOf[IndexerTest],
  classOf[RegionIndexerTest]
))
class TestSuite
