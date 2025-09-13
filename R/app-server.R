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
  # Shared reactive values for inter-module communication
  shared_values <- reactiveValues(
    # From data module
    data = NULL,
    data_source = NULL,
    column_names = NULL,
    upload_filename = NULL,
    
    # From fit module
    fitted_dist = NULL,
    selected_conc = NULL,
    selected_dists = NULL,
    selected_unit = NULL,
    fit_plot = NULL,
    gof_table = NULL,
    rescale = FALSE,
    fit_xaxis = NULL,
    fit_yaxis = NULL,
    fit_text_size = NULL,
    fit_plot_width = NULL,
    fit_plot_height = NULL,
    fit_plot_dpi = NULL,
    
    # From predict module
    predictions = NULL,
    threshold_values = NULL,
    model_average_plot = NULL,
    bootstrap_samples = NULL,
    predict_label = NULL,
    predict_colour = NULL,
    predict_shape = NULL,
    predict_xaxis = NULL,
    predict_yaxis = NULL,
    predict_title = NULL,
    predict_text_size = NULL,
    predict_label_size = NULL,
    predict_shift_x = NULL,
    predict_palette = NULL,
    predict_xmax = NULL,
    predict_xmin = NULL,
    transformation = NULL,
    xbreaks = NULL,
    show_hc = NULL,
    legend_colour = NULL,
    legend_shape = NULL,
    thresh_type = NULL,
    plot_width = NULL,
    plot_height = NULL,
    plot_dpi = NULL,
    
    # Global state
    translations = NULL,
    current_language = "English"
  )
  
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
  # report_mod <- mod_report_server("report_mod", shared_values, trans)
  # rcode_mod <- mod_rcode_server("rcode_mod", shared_values, trans)
  
  # --- get confidence intervals
  table_cl <- eventReactive(input$getCl, {
    dist <- fit_dist()
    waiter::waiter_show(html = waiting_screen_cl(), color = "rgba(44,62,80, 1)")
    nboot <- as.integer(gsub("(,|\\s)", "", input$bootSamp))
    if (input$thresh_type != "Concentration") {
      y <- ssd_hp_ave(dist, conc = thresh_rv$conc, nboot = nboot)
    } else {
      y <- ssd_hc_ave(dist, percent = thresh_rv$percent, nboot = nboot)
    }
    waiter::waiter_hide()
    y
  })
  
  estimate_time <- reactive({
    lang <- current_lang()
    if (lang == "english") {
      df <- data.frame(
        n = c("500", "1,000", "5,000", "10,000"),
        time = c("10 seconds", "20 seconds", "2 minutes", "5 minutes")
      )
    } else {
      df <- data.frame(
        n = c("500", "1 000", "5 000", "10 000"),
        time = c("10 secondes", "20 secondes", "2 minutes", "5 minutes")
      )
    }
    
    df[df$n == input$bootSamp, ]$time
  })
  
  ########### Outputs --------------------
  
  
  
  
  # dtopt <- reactive({
  #   url <- paste0("//cdn.datatables.net/plug-ins/1.10.11/i18n/", translation.value$lang, ".json")
  #   list(
  #     language = list(url = url),
  #     pageLength = 10
  #   )
  # })
  
  # --- render UI ----
  shinyjs::onclick(
    "linkFormatPredict",
    shinyjs::toggle(
      "divFormatPredict",
      anim = TRUE,
      animType = "slide",
      time = 0.2
    )
  )
  shinyjs::onclick(
    "linkPngFormatPredict",
    shinyjs::toggle(
      "divPngFormatPredict",
      anim = TRUE,
      animType = "slide",
      time = 0.2
    )
  )
  
  # params_list <- reactive({
  #   req(plot_model_average())
  #   toxicant <- input$toxicant
  #   data <- names_data()
  #   dists <- input$selectDist
  #   fit_plot <- plot_dist()
  #   fit_dist <- fit_dist()
  #   gof_table <- table_gof()
  #   model_average_plot <- plot_model_average()
  #   nboot <- as.integer(gsub("(,|\\s)", "", input$bootSamp))
  #   params <- list(
  #     toxicant = toxicant, data = data, dists = dists,
  #     fit_plot = fit_plot, fit_dist = fit_dist, gof_table = gof_table,
  #     model_average_plot = model_average_plot, nboot = nboot
  #   )
  #   params
  # })
  
  ########### Render UI Translations -------------------
  
  # Navigation titles and data controls now handled by client-side JavaScript
  output$ui_2select <- renderUI({
    selectizeInput(
      "selectDist",
      label = tr("ui_2dist", trans()),
      multiple = TRUE,
      choices = c(default.dists, extra.dists),
      selected = default.dists,
      options = list(
        "plugins" = list("remove_button"),
        "create" = TRUE,
        "persist" = FALSE
      )
    )
  })
  
  
  output$ui_unit <- renderUI({
    selectInput(
      "selectUnit",
      label = tr("ui_2unit", trans()),
      choices = units(),
      selected = units()[1]
    )
  })
  
  output$ui_2rescale <- renderUI({
    checkboxInput("rescale",
                  label = tr("ui_2rescale", trans()),
                  value = FALSE)
  })
  
  output$ui_2xlab <- renderUI({
    textInput("xaxis2",
              value = "Concentration",
              label = tr("ui_3xlab", trans()))
  })
  
  output$ui_2ylab <- renderUI({
    textInput(
      "yaxis2",
      value = tr("ui_2ploty", trans()),
      label = tr("ui_3ylab", trans())
    )
  })
  
  output$ui_thresh_type <- renderUI({
    thresh_label <- tr("ui_3threshlabel", trans())
    thresh <- tr("ui_3thresh", trans())
    radioButtons(
      "thresh_type",
      label = span(`data-translate` = "ui_3threshlabel", "Threshold type"),
      choices = c("Concentration", thresh),
      selected = "Concentration",
      inline = TRUE
    )
  })
  
  output$ui_3thresh <- renderUI({
    req(input$thresh_type)
    if (input$thresh_type != "Concentration") {
      return(
        numericInput(
          "conc",
          label = span(`data-translate` = "ui_3byconc", "By concentration"),
          value = 1,
          min = 0,
          max = 100,
          step = 0.1,
          width = "100px"
        )
      )
    }
    div(inline(
      selectizeInput(
        "thresh",
        label = span(`data-translate` = "ui_3affecting", "% affecting"),
        choices = c(1, 5, 10, 20),
        options = list(create = TRUE, createFilter = "^[1-9][0-9]?$|^99$"),
        selected = 5,
        width = "100px"
      )
    ), inline(
      selectizeInput(
        "thresh_pc",
        label = span(`data-translate` = "ui_3protecting", "% protecting"),
        choices = c(99, 95, 90, 80),
        options = list(create = TRUE, createFilter = "^[1-9][0-9]?$|^99$"),
        selected = 95,
        width = "100px"
      )
    ))
  })
  
  
  
  
  output$ui_3pal <- renderUI({
    selectInput(
      "selectPalette",
      label = span(`data-translate` = "ui_3pal", "Palette"),
      choices = pals,
      selected = pals[2]
    )
  })
  
  output$ui_3pngopts <- renderUI({
    actionLink("linkPngFormatPredict", label = tr("ui_3pngopts", trans()))
  })
  
  output$ui_3model <- renderUI({
    h4(tr("ui_3model", trans()))
  })
  
  output$ui_3cl <- renderUI({
    h4(
      tr("ui_3cl", trans()),
      bslib::tooltip(
        bsicons::bs_icon("question-circle", style = "margin-left: 0.5rem; color: #6c757d; outline: none; border: none;"),
        uiOutput("ui_3help"),
        placement = "right"
      )
    )
  })
  
  output$ui_3help <- renderUI({
    tr("ui_3help", trans())
  })
  
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
