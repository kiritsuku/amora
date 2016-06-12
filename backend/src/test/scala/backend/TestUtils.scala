package backend

import org.junit.ComparisonFailure

object TestUtils {

  var debugTests: Boolean = true

  final implicit class Assert_===[A](private val actual: A) extends AnyVal {
    def ===(expected: A): Unit = {
      if (actual != expected) {
        (actual, expected) match {
          case (actual: Seq[_], expected: Seq[_]) ⇒
            val as = actual.map(_.toString).sorted.mkString("\n")
            val es = expected.map(_.toString).sorted.mkString("\n")
            throw new ComparisonFailure("", es, as)
          case (actual: Set[_], expected: Set[_]) ⇒
            val as = actual.toSeq.map(_.toString).sorted.mkString("\n")
            val es = expected.toSeq.map(_.toString).sorted.mkString("\n")
            throw new ComparisonFailure("", es, as)
          case _ ⇒
            throw new ComparisonFailure("", expected.toString, actual.toString)
        }
      }
    }
  }
}
