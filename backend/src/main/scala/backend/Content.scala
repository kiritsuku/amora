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
        div(id := "form1"),
        script(`type` := "text/javascript", raw("""
          $(document).ready(function() {
            $("#form1").alpaca({

              "schema": {
                  "title":"User Feedback",
                  "description":"What do you think about Alpaca?",
                  "type":"object",
                  "properties": {
                      "name": {
                          "type":"string",
                          "title":"Name",
                          "required":true
                      },
                      "feedback": {
                          "type":"string",
                          "title":"Feedback"
                      },
                      "ranking": {
                          "type":"string",
                          "title":"Ranking",
                          "enum":['excellent','ok','so so']
                      }
                  }
              },
              "options": {
                  "form": {
                      "attributes": {
                          "action": "http://httpbin.org/post",
                          "method": "post"
                      },
                      "buttons": {
                          "submit": {}
                      }
                  },
                  "helper": "Tell us what you think about Alpaca!",
                  "fields": {
                      "name": {
                          "size": 20,
                          "helper": "Please enter your name.",
                          "placeholder": "Enter your name"
                      },
                      "feedback" : {
                          "type": "textarea",
                          "rows": 5,
                          "cols": 40,
                          "helper": "Please enter your feedback."
                      },
                      "ranking": {
                          "type": "select",
                          "helper": "Select your ranking.",
                          "optionLabels": ["Awesome!", "It's Ok", "Hmm..."]
                      }
                  }
              }

            });
          });
        """))
      )
    )
  }

}
