package backend

import akka.http.scaladsl.model.HttpCharsets
import akka.http.scaladsl.model.MediaType
import akka.http.scaladsl.model.MediaTypes

object CustomContentTypes {

  private var mt = Set[MediaType.WithOpenCharset]()

  val `sparql-results+xml` = reg("sparql-results+xml")
  val `sparql-results+json` = reg("sparql-results+json")
  val n3 = MediaType.text("n3")
  val `sparql-query` = MediaType.applicationWithOpenCharset("sparql-query")
  mt += MediaTypes.`text/csv`
  mt += MediaTypes.`text/tab-separated-values`

  val `sparql-results+xml(UTF-8)` = `sparql-results+xml` withCharset HttpCharsets.`UTF-8`
  val `sparql-results+json(UTF-8)` = `sparql-results+json` withCharset HttpCharsets.`UTF-8`
  val `text/tab-separated-values(UTF-8)` = MediaTypes.`text/tab-separated-values` withCharset HttpCharsets.`UTF-8`
  val `text/n3(UTF-8)` = n3 withCharset HttpCharsets.`UTF-8`

  def allMediaTypes: Set[MediaType.WithOpenCharset] = mt

  private def reg(subType: String): MediaType.WithOpenCharset = {
    val mt = MediaType.applicationWithOpenCharset(subType)
    this.mt += mt
    mt
  }
}
