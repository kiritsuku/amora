package amora

package object api {

  private val impl = new ApiImpl

  def turtleModel(model: String): SparqlModel =
    impl.turtleModelFromString(model)

  final implicit class SparqlInterpolation(private val sc: StringContext) extends AnyVal {

    def turtleModel(args: Any*): SparqlModel = {
      impl.turtleModel(sc.parts.iterator, args.iterator)
    }

    def sparqlQuery(args: Any*): SparqlQuery = {
      impl.sparqlQuery(sc.parts.iterator, args.iterator)
    }

    def sparql(args: Any*): String = {
      impl.sparql(sc.parts.iterator, args.iterator)
    }
  }
}
