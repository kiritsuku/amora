package amora

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

import amora.converter.ClassfileConverterTest
import amora.backend.indexer.JavaBytecodeIndexerTest
import amora.backend.indexer.IndexerTest
import amora.backend.indexer.ScalaSourceIndexerTest
import amora.backend.indexer.ScalaSourceRegionIndexerTest
import amora.backend.services.FindUsagesTest
import amora.backend.services.FindDeclarationTest
import amora.api.ApiTest
import amora.backend.indexer.MultiProjectTest
import amora.backend.indexer.ScalaRefTest

@RunWith(classOf[Suite])
@SuiteClasses(Array(
  classOf[ClassfileConverterTest],
  classOf[JavaBytecodeIndexerTest],
  classOf[IndexerTest],
  classOf[ScalaSourceIndexerTest],
  classOf[ScalaSourceRegionIndexerTest],
  classOf[FindUsagesTest],
  classOf[FindDeclarationTest],
  classOf[ApiTest],
  classOf[MultiProjectTest],
  classOf[ScalaRefTest]
))
class TestSuite
