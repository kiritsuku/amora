package amora.backend

import akka.http.scaladsl.model.HttpCharsets
import akka.http.scaladsl.model.MediaType
import akka.http.scaladsl.model.MediaTypes

object CustomContentTypes {

  val `application/sparql-results+xml`               = MediaType.applicationWithOpenCharset("sparql-results+xml")
  val `application/sparql-results+json`              = MediaType.applicationWithOpenCharset("sparql-results+json")
  val `text/turtle`                                  = MediaType.text("turtle")
  val `application/sparql-query`                     = MediaType.applicationWithOpenCharset("sparql-query")

  val `application/sparql-results+xml(UTF-8)`        = `application/sparql-results+xml` withCharset HttpCharsets.`UTF-8`
  val `application/sparql-results+json(UTF-8)`       = `application/sparql-results+json` withCharset HttpCharsets.`UTF-8`
  val `text/turtle(UTF-8)`                           = `text/turtle` withCharset HttpCharsets.`UTF-8`
  val `text/tab-separated-values(UTF-8)`             = MediaTypes.`text/tab-separated-values` withCharset HttpCharsets.`UTF-8`
  val `application/sparql-query(UTF-8)`              = `application/sparql-query` withCharset HttpCharsets.`UTF-8`
  val `application/x-www-form-urlencoded(UTF-8)`     = MediaTypes.`application/x-www-form-urlencoded` withCharset HttpCharsets.`UTF-8`
}
