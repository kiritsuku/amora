package amora.backend.services

import amora.backend.Logger
import amora.converter.protocol._

class DottySourceIndexer(logger: Logger) extends ScalaService {

  def run(data: Seq[(String, String)]): String = {
    handleDottySource(Artifact(Project("testProject"), "o", "n", "v1"), data)

    response(s"""
      @prefix service:<http://amora.center/kb/Schema/0.1/Service/0.1/> .
      @prefix response:<http://amora.center/kb/ServiceResponse/0.1/> .
      <#this>
        a response: ;
        service:requestId <$requestId> ;
      .
    """)
  }

  private def handleDottySource(origin: Schema, data: Seq[(String, String)]): Unit = {
    ???
  }
}
