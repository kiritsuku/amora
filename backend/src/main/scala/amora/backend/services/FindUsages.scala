package amora.backend.services

class FindUsages extends ScalaService {

  def run(offset: Int): String = {
    val r = sparqlRequest(s"""
      prefix file:<http://amora.center/kb/amora/Schema/0.1/File/0.1/>
      prefix decl:<http://amora.center/kb/amora/Schema/0.1/Decl/0.1/>
      prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
      prefix amora:<http://amora.center/kb/amora/Schema/0.1/>

      select ?usageStart ?usageEnd where {
        # Find the identifier at an offset but exclude inferred references
        ?ident amora:posStart ?start; amora:posEnd ?end .
        filter ($offset >= ?start && $offset <= ?end && ?start != ?end)

        # ?ident can either be a Ref or a Decl
        {
          ?ident ref:refToDecl ?decl .
        }
        union
        {
          ?ident a decl: .
          bind(?ident as ?decl)
        }

        # ?usages can either be the Decl itself or all Refs
        {
          bind(?decl as ?usages) .
        }
        union
        {
          ?usages ref:refToDecl ?decl .
        }

        # Find the regions of all usages but exclude inferred references
        ?usages amora:posStart ?usageStart; amora:posEnd ?usageEnd .
        filter (?usageStart != ?usageEnd)

        # Find only the usages that are in the same file where ?ident is
        ?ident amora:owner+ ?identOwner .
        ?identOwner a file: .

        ?usages amora:owner+ ?usagesOwner .
        ?usagesOwner a file: .

        filter (?identOwner = ?usagesOwner)
      }
    """)
    val positions = r.map { row ⇒
      row.int("usageStart") → row.int("usageEnd")
    }

    response(s"""
      @prefix service:<http://amora.center/kb/Schema/Service/0.1/> .
      @prefix response:<http://amora.center/kb/ServiceResponse/0.1/> .
      @prefix decl:<http://amora.center/kb/amora/Schema/0.1/Decl/0.1/> .
      <#this>
        a response: ;
        service:requestId <$requestId> ;
        service:result [${
          positions.map {
            case (start, end) ⇒ s"""
          decl:posStart $start ;
          decl:posEnd $end ;
        ], ["""
          }.mkString
        }] ;
      .
    """)
  }
}
