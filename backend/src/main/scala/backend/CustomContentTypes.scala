package backend

import akka.http.scaladsl.model.HttpCharsets
import akka.http.scaladsl.model.MediaType
import akka.http.scaladsl.model.MediaTypes

object CustomContentTypes {

  private var mt = Set[MediaType.WithOpenCharset]()

  val `application/sparql-results+xml` = reg("sparql-results+xml")
  val `application/sparql-results+json` = reg("sparql-results+json")
  val `text/n3` = MediaType.text("n3")
  val `application/sparql-query` = MediaType.applicationWithOpenCharset("sparql-query")
  mt += MediaTypes.`text/csv`
  mt += MediaTypes.`text/tab-separated-values`

  val `application/sparql-results+xml(UTF-8)` = `application/sparql-results+xml` withCharset HttpCharsets.`UTF-8`
  val `application/sparql-results+json(UTF-8)` = `application/sparql-results+json` withCharset HttpCharsets.`UTF-8`
  val `text/tab-separated-values(UTF-8)` = MediaTypes.`text/tab-separated-values` withCharset HttpCharsets.`UTF-8`
  val `text/n3(UTF-8)` = `text/n3` withCharset HttpCharsets.`UTF-8`

  def allMediaTypes: Set[MediaType.WithOpenCharset] = mt

  private def reg(subType: String): MediaType.WithOpenCharset = {
    val mt = MediaType.applicationWithOpenCharset(subType)
    this.mt += mt
    mt
  }
}
