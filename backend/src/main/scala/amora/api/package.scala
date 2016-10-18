package amora

package object api {

  private val impl = new ApiImpl

  final implicit class SparqlInterpolation(private val sc: StringContext) extends AnyVal {

    def ttlModel(args: Any*): SparqlModel = {
      impl.ttlModel(sc.parts.iterator, args.iterator)
    }

    def sparqlQuery(args: Any*): SparqlQuery = {
      impl.sparqlQuery(sc.parts.iterator, args.iterator)
    }
  }
}
