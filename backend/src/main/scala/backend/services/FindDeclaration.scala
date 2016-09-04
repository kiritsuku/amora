package backend.services

class FindDeclaration extends ScalaService {

  def run(offset: Int): String = {
    val r = sparqlRequest(s"""
      prefix decl: <http://amora.center/kb/amora/Schema/0.1/Decl/0.1/>
      prefix ref: <http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>

      select ?declStart ?declEnd where {
        ?ref ref:posStart ?start; ref:posEnd ?end .
        filter ($offset >= ?start && $offset <= ?end)
        ?ref ref:refToDecl ?decl .
        ?decl decl:posStart ?declStart; decl:posEnd ?declEnd .
      }
    """)
    import scala.collection.JavaConverters._
    val (start, end) = r.asScala.map { qs ⇒
      qs.get("declStart").asLiteral().getInt → qs.get("declEnd").asLiteral().getInt
    }.toSeq.head

    response(s"""
      @prefix service: <http://amora.center/kb/Schema/Service/0.1/>
      @prefix response: <http://amora.center/kb/ServiceResponse/0.1/>
      @prefix decl: <http://amora.center/kb/amora/Schema/0.1/Decl/0.1/>
      <#this>
        a response: ;
        service:requestId <$requestId> ;
        service:result [
          decl:posStart $start ;
          decl:posEnd $end ;
        ] ;
      .
    """)
  }
}
