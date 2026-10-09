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

# Data Module UI
mod_data_ui <- function(id) {
  ns <- NS(id)

  aside <- card(card_body(
    gap = "1rem",
    div(
      class = "small text-body-secondary",
      span(`data-translate` = "ui_1choose", "Choose one of the following options:")
    ),
    div(
      class = "d-flex flex-column gap-3",
      button(
        ns("demoData"),
        span(
          span(`data-translate` = "ui_1data", "1. Use "),
          span(`data-translate` = "ui_1data2", "boron dataset")
        ),
        icon = "flask",
        variant = "soft",
        class = "w-100"
      ),
      fileInput(
        ns("uploadData"),
        label = span(`data-translate` = "ui_1csv", "2. Upload CSV file"),
        buttonLabel = span(class = "d-inline-flex align-items-center gap-2", lucide("upload"), "CSV"),
        placeholder = "...",
        accept = c(".csv"),
        width = "100%"
      ) |>
        tagAppendAttributes(class = "mb-0"),
      textInput(
        ns("toxicant"),
        label = span(`data-translate` = "ui_1toxname", "Toxicant name (optional)"),
        value = "",
        placeholder = "",
        width = "100%"
      ) |>
        tagAppendAttributes(class = "mb-0")
    )
  ))

  main <- tagList(
    page_header(
      span(`data-translate` = "ui_tabdata", "Provide data") |>
        shinyhelper::helper(type = "markdown", content = "dataTab", size = "l", colour = color_primary, buttonLabel = "OK"),
      conditionalPanel(
        condition = paste_js("has_data", ns),
        step_button(ns("continue"), "ui_continue_fit", "Continue to fit")
      )
    ),
    div(
      class = "d-flex flex-column gap-4",
      # In the page from the start, so it shows before the server's first
      # response; hidden in the browser once data are loaded.
      conditionalPanel(
        condition = sprintf("!%s", paste_js("has_data", ns)),
        welcome_card(ns("guide"))
      ),
      conditionalPanel(
        condition = paste_js("has_data", ns),
        notice(
          "info",
          span(
            `data-translate` = "ui_1note",
            "Note: the app is designed to handle one chemical at a time. Each species should not have more than one concentration value."
          ),
          tone = "muted"
        )
      ),
      accordion(
        id = ns("sections"),
        open = FALSE,
        accordion_panel(
          title = span(`data-translate` = "ui_1preview", "Preview chosen dataset"),
          value = "preview",
          icon = lucide("table"),
          conditionalPanel(
            condition = paste_js("has_data", ns),
            reactable::reactableOutput(ns("viewUpload"))
          ),
          conditionalPanel(
            condition = sprintf("!%s", paste_js("has_data", ns)),
            div(class = "small text-body-secondary", span(`data-translate` = "ui_hintdata", "You have not added a dataset."))
          )
        ),
        accordion_panel(
          title = span(`data-translate` = "ui_1table", "3. Fill out table below:"),
          value = "data_table",
          icon = lucide("sliders-horizontal"),
          rhandsontable::rHandsontableOutput(ns("handson")),
          div(
            class = "mt-3",
            button(
              ns("handson_done"),
              span(`data-translate` = "ui_update_data", "Update"),
              icon = "refresh-cw",
              variant = "outline"
            )
          )
        )
      )
    )
  )

  step_layout(aside, main)
}

# Data Module Server
mod_data_server <- function(id, translations, lang, shared_toxicant_name = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    active_source <- reactiveVal("none")

    # Update toxicant input when shared value changes from another module
    if (!is.null(shared_toxicant_name)) {
      observe({
        toxicant_name <- shared_toxicant_name()
        if (!is.null(toxicant_name) && toxicant_name != "" &&
            toxicant_name != input$toxicant) {
          updateTextInput(
            session,
            "toxicant",
            value = toxicant_name
          )
        }
      }) |>
        bindEvent(shared_toxicant_name())

      # Update shared value when this module's input changes
      observe({
        shared_toxicant_name(input$toxicant)
      }) |>
        bindEvent(input$toxicant)
    }

    demo_data <- reactive({
      df <- boron.data
      trans <- translations()
      spp <- tr("ui_1htspp", trans)
      conc <- tr("ui_1htconc", trans)
      grp <- tr("ui_1htgrp", trans)

      # Keep only Species, Conc, and Group columns
      df <- df[, c("Species", "Conc", "Group")]
      colnames(df) <- c(spp, conc, grp)
      df
    }) |>
      bindEvent(translations(), input$demoData)

    upload_data <- reactive({
      data <- input$uploadData
      if (!grepl(".csv", data$name, fixed = TRUE)) {
        showNotification(
          "We're not sure what to do with that file type. Please upload a CSV file.",
          type = "error",
          duration = 10
        )
        return(NULL)
      }

      # Try to read CSV with graceful error handling
      result <- tryCatch(
        {
          suppressMessages(readr::read_csv(
            data$datapath,
            show_col_types = FALSE
          ))
        },
        error = function(e) {
          showNotification(
            ui = div(
              strong("Could not read CSV file"),
              br(),
              "Error: ",
              as.character(e$message)
            ),
            type = "error",
            duration = NULL
          )
          return(NULL)
        }
      )

      return(result)
    }) |>
      bindEvent(input$uploadData)

    handson_data <- reactive({
      if (!is.null(input$handson)) {
        trans <- translations()
        df <- rhandsontable::hot_to_r(input$handson)
        colnames(df) <- c(
          tr("ui_1htconc", trans),
          tr("ui_1htspp", trans),
          tr("ui_1htgrp", trans)
        )
        dplyr::mutate_if(df, is.factor, as.character)
      } else {
        data.frame(
          "Concentration" = rep(NA_real_, 10),
          "Species" = rep(NA_character_, 10),
          "Group" = rep(NA_character_, 10)
        )
      }
    })

    handson_data_done <- reactive({
      handson_data()
    }) |>
      bindEvent(input$handson_done, translations())

    observe({
      active_source("upload")
    }) |>
      bindEvent(input$uploadData)

    observe({
      active_source("demo")
    }) |>
      bindEvent(input$demoData)

    # New data open the preview, whichever way they were provided.
    observe(accordion_panel_open("sections", "preview")) |>
      bindEvent(input$demoData, input$uploadData, input$handson_done, ignoreInit = TRUE)

    observe({
      active_source("handson")
    }) |>
      bindEvent(input$handson_done)

    current_data <- reactive({
      switch(
        active_source(),
        "demo" = demo_data(),
        "upload" = upload_data(),
        "handson" = handson_data_done(),
        NULL
      )
    })

    clean_data <- reactive({
      data <- current_data()
      req(data)
      req(!is.null(data))
      req(is.data.frame(data))

      if (length(data)) {
        data <- clean_ssd_data(data)
      }
      data
    })

    names_data <- reactive({
      data <- clean_data()
      names(data) <- make.names(names(data))
      data
    })

    has_data <- reactive({
      data <- tryCatch(
        {
          names_data()
        },
        error = function(e) NULL
      )

      if (is.null(data) || nrow(data) == 0) {
        return(FALSE)
      }

      if (active_source() == "handson" && all(is.na(data[[1]]))) {
        return(FALSE)
      }

      TRUE
    })

    output$has_data <- has_data
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    output$handson <- rhandsontable::renderRHandsontable({
      x <- handson_data()
      if (!is.null(x)) {
        rhandsontable::rhandsontable(x, useTypes = FALSE, stretchH = "all")
      }
    })

    output$viewUpload <- reactable::renderReactable({
      data <- current_data()
      req(data)
      app_table(
        as.data.frame(data),
        lang = lang(),
        searchable = TRUE,
        defaultPageSize = 10,
        paginationType = "simple"
      )
    })
    # The preview is in a collapsed accordion panel, and Shiny does not resume
    # a suspended output when an accordion panel opens.
    outputOptions(output, "viewUpload", suspendWhenHidden = FALSE)

    observe_step_button(input, "continue", "fit")
    observe({
      nav_select("main_nav", "help", session = session$rootScope())
      nav_select("help_page", "guide", session = session$rootScope())
    }) |>
      bindEvent(input$guide)

    return(
      list(
        data = names_data,
        current_data = current_data,
        clean_data = clean_data,
        data_cols = reactive({
          names(clean_data())
        }),
        has_data = has_data,
        toxicant_name = reactive({
          input$toxicant
        })
      )
    )
  })
}
