package amora.backend

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

import amora.converter.ClassfileConverterTest
import amora.converter.ScalacConverterTest
import amora.backend.indexer.JavaBytecodeIndexerTest
import amora.backend.indexer.IndexerTest
import amora.backend.indexer.ScalaSourceIndexerTest
import amora.backend.indexer.ScalaSourceRegionIndexerTest
import amora.backend.services.FindUsagesTest
import amora.backend.services.FindDeclarationTest

@RunWith(classOf[Suite])
@SuiteClasses(Array(
  classOf[ScalacConverterTest],
  classOf[ClassfileConverterTest],
  classOf[JavaBytecodeIndexerTest],
  classOf[IndexerTest],
  classOf[ScalaSourceIndexerTest],
  classOf[ScalaSourceRegionIndexerTest],
  classOf[FindUsagesTest],
  classOf[FindDeclarationTest]
))
class TestSuite
