package amora.backend.services

class FindDeclaration extends ScalaService {

  def run(offset: Int): String = {
    val r = sparqlRequest(s"""
      prefix decl:<http://amora.center/kb/amora/Schema/Decl/>
      prefix ref:<http://amora.center/kb/amora/Schema/Ref/>
      prefix amora:<http://amora.center/kb/amora/Schema/>

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
    val position = r.map { row ⇒
      row.int("declStart") → row.int("declEnd")
    }

    response(s"""
      @prefix service:<http://amora.center/kb/Schema/Service/> .
      @prefix response:<http://amora.center/kb/ServiceResponse/> .
      @prefix decl:<http://amora.center/kb/amora/Schema/Decl/> .
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
