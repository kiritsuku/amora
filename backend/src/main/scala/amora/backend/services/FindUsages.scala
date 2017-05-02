package amora.backend.services

class FindUsages extends ScalaService {

  def run(offset: Int): String = {
    val r = sparqlRequest(s"""
      prefix File:<http://amora.center/kb/amora/Schema/File/>
      prefix Decl:<http://amora.center/kb/amora/Schema/Decl/>
      prefix Ref:<http://amora.center/kb/amora/Schema/Ref/>
      prefix Schema:<http://amora.center/kb/amora/Schema/>

      select ?usageStart ?usageEnd where {
        # Find the identifier at an offset but exclude inferred references
        ?ident Schema:posStart ?start; Schema:posEnd ?end .
        filter ($offset >= ?start && $offset <= ?end && ?start != ?end)

        # ?ident can either be a Ref or a Decl
        {
          ?ident Ref:refToDecl ?decl .
        }
        union
        {
          ?ident a Decl: .
          bind(?ident as ?decl)
        }

        # ?usages can either be the Decl itself or all Refs
        {
          bind(?decl as ?usages) .
        }
        union
        {
          ?usages Ref:refToDecl ?decl .
        }

        # Find the regions of all usages but exclude inferred references
        ?usages Schema:posStart ?usageStart; Schema:posEnd ?usageEnd .
        filter (?usageStart != ?usageEnd)

        # Find only the usages that are in the same file where ?ident is
        ?ident Schema:owner+ ?identOwner .
        ?identOwner a File: .

        ?usages Schema:owner+ ?usagesOwner .
        ?usagesOwner a File: .

        filter (?identOwner = ?usagesOwner)
      }
    """)
    val positions = r.map { row ⇒
      row.int("usageStart") → row.int("usageEnd")
    }

    response(s"""
      @prefix Service:<http://amora.center/kb/Schema/Service/> .
      @prefix Response:<http://amora.center/kb/ServiceResponse/> .
      @prefix Decl:<http://amora.center/kb/amora/Schema/Decl/> .
      <#this>
        a Response: ;
        Service:requestId <$requestId> ;
        Service:result [${
          positions.map {
            case (start, end) ⇒ s"""
          Decl:posStart $start ;
          Decl:posEnd $end ;
        ], ["""
          }.mkString
        }] ;
      .
    """)
  }
}
