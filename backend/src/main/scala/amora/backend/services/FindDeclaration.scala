package amora.backend.services

class FindDeclaration extends ScalaService {

  def run(offset: Int): String = {
    val r = sparqlRequest(s"""
      prefix Decl:<http://amora.center/kb/amora/Schema/Decl/>
      prefix Ref:<http://amora.center/kb/amora/Schema/Ref/>
      prefix Schema:<http://amora.center/kb/amora/Schema/>

      select ?declStart ?declEnd where {
        ?ident Schema:posStart ?start; Schema:posEnd ?end .
        filter ($offset >= ?start && $offset <= ?end)

        # ?ident can either be a Ref or a Decl
        {
          ?ident Ref:refToDecl ?decl .
        }
        union
        {
          ?ident a Decl: .
          bind(?ident as ?decl)
        }

        ?decl Decl:posStart ?declStart; Decl:posEnd ?declEnd .
      }
      limit 1
    """)
    val position = r.map { row ⇒
      row.int("declStart") → row.int("declEnd")
    }

    response(s"""
      @prefix Service:<http://amora.center/kb/Schema/Service/> .
      @prefix Response:<http://amora.center/kb/ServiceResponse/> .
      @prefix Decl:<http://amora.center/kb/amora/Schema/Decl/> .
      <#this>
        a Response: ;
        Service:requestId <$requestId> ;
        Service:result ${
          position.map {
            case (start, end) ⇒ s"""[
          Decl:posStart $start ;
          Decl:posEnd $end ;
        ] ;"""
          }.headOption.getOrElse("[]")
        }
      .
    """)
  }
}
