package backend.services

class FindUsages extends ScalaService {

  def run(offset: Int): String = {
    val r = sparqlRequest(s"""
      prefix decl:<http://amora.center/kb/amora/Schema/0.1/Decl/0.1/>
      prefix ref:<http://amora.center/kb/amora/Schema/0.1/Ref/0.1/>
      prefix amora:<http://amora.center/kb/amora/Schema/0.1/>

      select ?uStart ?uEnd where {
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

        ?usages amora:posStart ?uStart; amora:posEnd ?uEnd .
        filter (?uStart != ?uEnd)
      }
    """)
    import scala.collection.JavaConverters._
    val positions = r.asScala.map { qs ⇒
      qs.get("uStart").asLiteral().getInt → qs.get("uEnd").asLiteral().getInt
    }

    response(s"""
      @prefix service:<http://amora.center/kb/Schema/Service/0.1/> .
      @prefix response:<http://amora.center/kb/ServiceResponse/0.1/> .
      @prefix decl:<http://amora.center/kb/amora/Schema/0.1/Decl/0.1/> .
      <#this>
        a response: ;
        service:requestId <$requestId> ;
        service:result ([${
          positions.map {
            case (start, end) ⇒ s"""
          decl:posStart $start ;
          decl:posEnd $end ;
        ] ["""
          }.mkString
        }]) ;
      .
    """)
  }
}
