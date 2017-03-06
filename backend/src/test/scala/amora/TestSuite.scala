package amora

import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

import amora.converter.ClassfileConverterTest
import amora.backend.indexer.JavaBytecodeIndexerTest
import amora.backend.indexer.IndexerTest
import amora.backend.indexer.ScalaSchemaTest
import amora.backend.indexer.ScalaDeclTest
import amora.backend.services.FindUsagesTest
import amora.backend.services.FindDeclarationTest
import amora.api.ApiTest
import amora.backend.indexer.MultiProjectTest
import amora.backend.indexer.ScalaRefTest
import amora.nlp.NlqTest
import amora.backend.indexer.FlagTest

@RunWith(classOf[Suite])
@SuiteClasses(Array(
  classOf[ClassfileConverterTest],
  classOf[JavaBytecodeIndexerTest],
  classOf[IndexerTest],
  classOf[ScalaSchemaTest],
  classOf[ScalaDeclTest],
  classOf[FindUsagesTest],
  classOf[FindDeclarationTest],
  classOf[ApiTest],
  classOf[MultiProjectTest],
  classOf[ScalaRefTest],
  classOf[NlqTest],
  classOf[FlagTest]
))
class TestSuite
