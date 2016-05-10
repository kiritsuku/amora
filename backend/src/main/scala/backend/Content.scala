package backend

object Content {
  import scalatags.Text.all._
  import scalatags.Text.tags2

  def indexPage(cssDeps: Seq[String], jsDeps: Seq[String]): String = {
    "<!DOCTYPE html>" + html(
      head(
        meta(charset := "UTF-8"),
        tags2.title("IDE research"),
        for (d <- cssDeps) yield link(rel := "stylesheet", `type` := "text/css", href := d)
      ),
      body(
        script(`type` := "text/javascript", src := "ui-jsdeps.js", onload := "window.$ = window.jQuery = module.exports;"),
        for (d <- jsDeps) yield script(`type` := "text/javascript", src := d)
      )
    )
  }

  def sparql(cssDeps: Seq[String], jsDeps: Seq[String]): String = {
    val q = """
      |PREFIX c:<http://test.model/>
      |PREFIX s:<http://schema.org/>
      |
      |SELECT * WHERE {
      |  ?s ?p ?o .
      |}
    """.stripMargin.trim.replace("\n", "\\n")

    "<!DOCTYPE html>" + html(
      head(
        meta(charset := "UTF-8"),
        tags2.title("SPARQL GUI"),
        for (d <- cssDeps) yield link(rel := "stylesheet", `type` := "text/css", href := d)
      ),
      body(
        div(id := "yasgui"),
        for (d <- jsDeps) yield script(`type` := "text/javascript", src := d),
        script(`type` := "text/javascript", raw(s"""
          YASGUI.YASQE.defaults.sparql.endpoint = "http://localhost:9999/sparql";
          YASGUI.YASQE.defaults.value = "$q";
          var yasgui = YASGUI(document.getElementById("yasgui"));
        """))
      )
    )
  }

  def addJsonPage(cssDeps: Seq[String], jsDeps: Seq[String]): String = {
    "<!DOCTYPE html>" + html(
      head(
        meta(charset := "UTF-8"),
        tags2.title("Add data"),
        for (d <- cssDeps) yield link(rel := "stylesheet", `type` := "text/css", href := d),
        for (d <- jsDeps) yield script(`type` := "text/javascript", src := d)
      ),
      body(
        div(id := "form"),
        script(`type` := "text/javascript", raw(s"""
          $$(document).ready(function() {
            $$("#form").alpaca(
              ${schemas.artifacts}
            );
          });
        """))
      )
    )
  }

  def queuePage(items: Seq[Int]): String = {
    "<!DOCTYPE html>" + html(
      head(
        meta(charset := "UTF-8"),
        tags2.title("Indexer queue")
      ),
      body(
        h4("Queue items"),
        ul(
          if (items.isEmpty)
            li("No items")
          else
            for (i ← items) yield li(a(href := s"//localhost:9999/queue?item=$i", s"Item $i"))
        )
      )
    )
  }

  def itemPage(item: Int, logger: Logger): String = {
    "<!DOCTYPE html>" + html(
      head(
        meta(charset := "UTF-8"),
        tags2.title("Queue item")
      ),
      body(
        h4(s"Queue item $item")
      )
    )
  }

  def kbPage(cssDeps: Seq[String], jsDeps: Seq[String]): String = {
    "<!DOCTYPE html>" + html(
      head(
        meta(charset := "UTF-8"),
        tags2.title("Knowledge Base"),
        for (d <- cssDeps) yield link(rel := "stylesheet", `type` := "text/css", href := d)
      ),
      body(
        for (d <- jsDeps) yield script(`type` := "text/javascript", src := d)
      )
    )
  }

  object schemas {

    val artifacts = """{
      "schema": {
        "title":"Artifact Indexing",
        "description":"Specify an artifact to index",
        "type":"object",
        "properties": {
          "tpe": {
            "type":"string",
            "hidden": true
          },
          "artifacts": {
            "type": "array",
            "items": {
              "type": "object",
              "properties": {
                "organization": {
                  "type":"string",
                  "title":"Organization",
                  "required":true
                },
                "name": {
                  "type":"string",
                  "title":"Name",
                  "required":true
                },
                "version": {
                  "type":"string",
                  "title":"Version",
                  "required":true
                }
              }
            }
          }
        }
      },
      "options": {
        "form": {
          "buttons": {
            "submit": {
              "click": function() {
                // we need to do some logic here but I couldn't find out a better
                // way to combine alpace with Scala.js code, therefore we just call
                // the Scala code here directly.
                frontend.webui.Main().handleFormSubmit(this);
              }
            },
            "reset": {}
          }
        },
        "fields": {
          "tpe": {
            "hidden": true
          },
          "artifacts": {
            "toolbarSticky": true,
            "fields": {
              "item": {
                "fields": {
                  "organization": {
                    "size": 20,
                    "placeholder": "Enter the organization of the artifact"
                  },
                  "name": {
                    "size": 20,
                    "placeholder": "Enter the name of the artifact"
                  },
                  "version": {
                    "size": 20,
                    "placeholder": "Enter the version of the artifact"
                  }
                }
              }
            }
          }
        }
      },
      "data": {
        "tpe": "artifact"
      }
    }"""

    val all = Map(
      "artifacts" → artifacts
    )

  }

}
