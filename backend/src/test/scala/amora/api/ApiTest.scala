package amora.api

import org.junit.Test

class ApiTest {
  import amora.TestUtils._

  @Test
  def can_read_ttl_model() = {
    val m = ttlModel"""
      <#a>
        <#b> [
          <#c1> "c1" ;
          <#c2> "c2" ;
        ] ;
      .
    """
    sparqlQuery"""select (count(*) as ?count) where {?s ?p ?o}""".runOnModel(m).map {
      _.int("count")
    }.head === 3
  }

  @Test
  def can_run_same_query_result_set_multiple_times() = {
    val m = ttlModel"""
      <#a>
        <#b> [
          <#c1> "c1" ;
          <#c2> "c2" ;
        ] ;
      .
    """
    val rs = sparqlQuery"""select (count(*) as ?count) where {?s ?p ?o}""".runOnModel(m)
    rs.map { _.int("count") }.length === 1
    rs.map { _.int("count") }.head === 3
    rs.map { _.int("count") }.head === 3
  }
}
