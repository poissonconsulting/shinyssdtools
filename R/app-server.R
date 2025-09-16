# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
app_server <- function(input, output, session) {
  
  # --- Translations
  current_lang <- reactive({
    ifelse (input$english > input$french | input$english == input$french, "english", "french")
  })
  
  trans <- reactive({
    translations$trans <- translations[[current_lang()]]
    translations
  }) %>% 
    bindEvent(current_lang())
  
  client_translations <- reactive({
    id <- unique(translations$id)
    trans_data <- trans()
    
    translations <- sapply(id, function(x)
      tr(x, trans_data))
    names(translations) <- id
    as.list(translations)
  })
  
  # Send to client
  observe({
    session$sendCustomMessage(
      "updateTranslations",
      list(translations = client_translations(), language = current_lang())
    )
  }) %>% 
    bindEvent(client_translations())
  
  
  # Module Server Calls -----------------------------------------------------
  
  # Call module servers with shared values
  data_mod <- mod_data_server("data_mod", trans, current_lang)
  fit_mod <- mod_fit_server("fit_mod", trans, data_mod, main_nav = reactive({input$main_nav}))
  predict_mod <- mod_predict_server("predict_mod", trans, current_lang, data_mod, fit_mod, main_nav = reactive({input$main_nav}))
  report_mod <- mod_report_server("report_mod", trans, current_lang, data_mod, fit_mod, predict_mod)
  # rcode_mod <- mod_rcode_server("rcode_mod", shared_values, trans)

  
  output$ui_5format <- renderUI({
    radioButtons("report_format", "Report format")
  })
  
  output$ui_about <- renderUI({
    lang <- current_lang()
    ver <- paste("ssdtools version:", utils::packageVersion("ssdtools"))
    sver <- paste("shinyssdtools version:",
                  utils::packageVersion("shinyssdtools"))
    if (lang == "english") {
      return({
        tagList(p(ver), p(sver), includeHTML(
          system.file("extdata/about-en.html", package = "shinyssdtools")
        ))
      })
    } else {
      return({
        tagList(p(ver), p(sver), includeHTML(
          system.file("extdata/about-fr.html", package = "shinyssdtools")
        ))
      })
    }
  })
  
  output$ui_userguide <- renderUI({
    lang <- current_lang()
    if (lang == "english") {
      return(includeHTML(
        system.file(package = "shinyssdtools", "extdata/user-en.html")
      ))
    }
    includeHTML(system.file(package = "shinyssdtools", "extdata/user-fr.html"))
  })
}
