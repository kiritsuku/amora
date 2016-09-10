package backend.services

class FindDeclaration extends ScalaService {

  def run(offset: Int): String = {
    val r = sparqlRequest(s"""
      prefix decl:<http://amora.center/kb/amora/Schema/0.1/Decl/0.1/>
      prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
      prefix amora:<http://amora.center/kb/amora/Schema/0.1/>

      select ?declStart ?declEnd where {
        ?ident amora:posStart ?start; amora:posEnd ?end .
        filter ($offset >= ?start && $offset <= ?end)

        # ?ident can either be a Ref or a Decl
        {
          ?ident ref:refToDecl ?decl .
        }
        union
        {
          ?ident a decl: .
          bind(?ident as ?decl)
        }

        ?decl decl:posStart ?declStart; decl:posEnd ?declEnd .
      }
      limit 1
    """)
    import scala.collection.JavaConverters._
    val position = r.asScala.map { qs ⇒
      qs.get("declStart").asLiteral().getInt → qs.get("declEnd").asLiteral().getInt
    }.toSeq

    response(s"""
      @prefix service:<http://amora.center/kb/Schema/Service/0.1/> .
      @prefix response:<http://amora.center/kb/ServiceResponse/0.1/> .
      @prefix decl:<http://amora.center/kb/amora/Schema/0.1/Decl/0.1/> .
      <#this>
        a response: ;
        service:requestId <$requestId> ;
        service:result ${
          position.map {
            case (start, end) ⇒ s"""[
          decl:posStart $start ;
          decl:posEnd $end ;
        ] ;"""
          }.headOption.getOrElse("[]")
        }
      .
    """)
  }
}
