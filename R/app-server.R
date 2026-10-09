# Copyright 2015-2025 Province of British Columbia
# Copyright 2021 Environment and Climate Change Canada
# Copyright 2023-2025 Australian Government Department of Climate Change,
# Energy, the Environment and Water
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.
app_server <- function(input, output, session) {
  # --- Translations
  # The language last chosen in the Language menu.
  current_lang <- reactiveVal("english")
  observe(current_lang("english")) |> bindEvent(input$english)
  observe(current_lang("french")) |> bindEvent(input$french)
  observe(current_lang("spanish")) |> bindEvent(input$spanish)

  # Set up shinyhelper with language-specific help files
  observe({
    lang_dir <- switch(
      current_lang(),
      "english" = "en",
      "french" = "fr",
      "spanish" = "es"
    )
    shinyhelper::observe_helpers(
      help_dir = system.file(
        paste("helpfiles", lang_dir, sep = "_"),
        package = "shinyssdtools"
      )
    )
  })

  # shinyhelper::observe_helpers(
  #   help_dir = system.file("helpfiles", package = "shinyssdtools")
  # )

  trans <- reactive({
    translations$trans <- translations[[current_lang()]]
    translations
  }) |>
    bindEvent(current_lang())

  client_translations <- reactive({
    id <- unique(translations$id)
    trans_data <- trans()

    translations <- sapply(id, function(x) {
      tr(x, trans_data)
    })
    names(translations) <- id
    as.list(translations)
  })

  # Send to client
  observe({
    session$sendCustomMessage(
      "updateTranslations",
      list(translations = client_translations(), language = current_lang())
    )
  }) |>
    bindEvent(client_translations())

  # --- Number formatting
  big_mark <- reactive({
    switch(
      current_lang(),
      "french" = " ",
      "spanish" = ".",
      ","  # Default for English
    )
  }) |>
    bindEvent(current_lang())

  decimal_mark <- reactive({
    switch(
      current_lang(),
      "french" = ",",
      "spanish" = ",",
      "."  # Default for English
    )
  }) |>
    bindEvent(current_lang())

  # Module Server Calls -----------------------------------------------------

  # Shared toxicant name across modules
  shared_toxicant_name <- reactiveVal("")

  # Call module servers with shared values
  data_mod <- mod_data_server("data_mod", trans, current_lang, shared_toxicant_name)
  fit_mod <- mod_fit_server(
    "fit_mod",
    trans,
    current_lang,
    data_mod,
    big_mark,
    decimal_mark,
    main_nav = reactive({
      input$main_nav
    })
  )
  predict_mod <- mod_predict_server(
    "predict_mod",
    trans,
    current_lang,
    data_mod,
    fit_mod,
    big_mark,
    decimal_mark
  )
  export_mod <- mod_export_server(
    "export_mod",
    trans,
    current_lang,
    data_mod,
    fit_mod,
    predict_mod,
    main_nav = reactive(input$main_nav),
    code = function() rcode_mod$code()
  )
  # The generated script saves plots with the Export step's PNG settings.
  for (setting in c("width", "height", "dpi")) {
    fit_mod[[setting]] <- export_mod[[setting]]
    predict_mod[[setting]] <- export_mod[[setting]]
  }
  rcode_mod <- mod_rcode_server(
    "rcode_mod",
    trans,
    data_mod,
    fit_mod,
    predict_mod
  )

  # The steps the user has opened: a step computed in the background is not
  # done until it has been seen.
  visited <- reactiveVal(character())
  observe(visited(union(visited(), input$main_nav))) |>
    bindEvent(input$main_nav)

  # The states of the step markers (step_title()): "done" once a step has
  # been opened and has a result, "busy" while it computes in the
  # background, else "todo".
  step_state <- function(step, done, busy = function() FALSE) {
    reactive({
      if (isTRUE(busy())) {
        return("busy")
      }
      has_result <- isTRUE(tryCatch(done(), error = function(e) FALSE))
      if (has_result && step %in% visited()) "done" else "todo"
    })
  }
  output$mark_data <- step_state("data", data_mod$has_data)
  output$mark_fit <- step_state("fit", fit_mod$has_fit)
  output$mark_predict <- step_state("predict", predict_mod$has_predict, predict_mod$cl_running)
  output$mark_export <- step_state("export", export_mod$has_preview, export_mod$running)
  for (step in c("data", "fit", "predict", "export")) {
    outputOptions(output, paste0("mark_", step), suspendWhenHidden = FALSE)
  }

  # The rendered About HTML, the source of the Methods and About pages.
  about_html <- reactive({
    file_suffix <- switch(current_lang(), "french" = "fr", "spanish" = "es", "en")
    file_path <- system.file(package = "shinyssdtools", paste0("extdata/about-", file_suffix, ".html"))
    # Fall back to English if translation doesn't exist
    if (!nzchar(file_path)) {
      file_path <- system.file(package = "shinyssdtools", "extdata/about-en.html")
    }
    paste(readLines(file_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  })

  output$ui_methods <- renderUI(methods_page(about_html()))
  output$ui_about <- renderUI(about_page(about_html()))

  output$ui_userguide <- renderUI({
    lang <- current_lang()
    file_suffix <- switch(
      lang,
      "english" = "en",
      "french" = "fr",
      "spanish" = "es",
      "en"  # Default to English
    )
    file_path <- system.file(
      package = "shinyssdtools",
      paste0("extdata/user-", file_suffix, ".html")
    )

    # Fall back to English if translation doesn't exist
    if (!file.exists(file_path) || file_path == "") {
      file_path <- system.file(package = "shinyssdtools", "extdata/user-en.html")
    }

    includeHTML(file_path)
  }) |>
    bindEvent(current_lang())
}
