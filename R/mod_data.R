# Data Module UI
mod_data_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    padding = "1rem",
    gap = "1rem",
    sidebar = sidebar(
      width = 400,
      div(
        h5(span(`data-translate` = "ui_tabdata", "Provide data")),
      ) %>%
        shinyhelper::helper(
          type = "markdown",
          content = "dataTab",
          size = "l",
          colour = "#759dbe",
          buttonLabel = "OK"
        ),
      span(
        `data-translate` = "ui_1choose",
        "Choose one of the following options:",
        id = ns("chooseOptions")
      ),
      p(
        span(
          span(`data-translate` = "ui_1data", "1. Use "),
          actionLink(
            ns("demoData"),
            span(`data-translate` = "ui_1data2", "boron dataset"),
            icon = icon("table")
          )
        )
      ),

      # CSV Upload Section
      fileInput(
        ns("uploadData"),
        buttonLabel = span(tagList(icon("upload"), "csv")),
        label = span(
          span(`data-translate` = "ui_1csv", "2. Upload CSV file"),
        ),
        placeholder = "...",
        accept = c(".csv")
      ),

      # Data Table Section
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          title = span(
            span(`data-translate` = "ui_1table", "3. Fill out table below:"),
          ),
          value = "data_table",
          rhandsontable::rHandsontableOutput(ns("handson")),
          div(
            class = "mt-3",
            actionButton(
              ns("handson_done"),
              label = tagList(
                icon("refresh", class = "text-white me-1"),
                span(`data-translate` = "ui_update_data", "Update")
              ),
              class = "btn-primary w-100"
            )
          )
        )
      )
    ),

    conditionalPanel(
      condition = glue::glue(
        "input.main_nav == 'data' && {paste_js('has_data', ns)} == true"
      ),
      card(
        card_header(span(
          `data-translate` = "ui_1preview",
          "Preview chosen dataset"
        )),
        card_body(DT::DTOutput(ns("viewUpload")))
      ),
      card(
        class = "mt-3",
        card_body(span(
          `data-translate` = "ui_1note",
          "Note: the app is designed to handle one chemical at a time. Each species should not have more than one concentration value."
        ))
      )
    )
  )
}

# Data Module Server
mod_data_server <- function(id, translations, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    active_source <- reactiveVal("none")

    demo_data <- reactive({
      df <- boron.data
      trans <- translations()
      conc <- tr("ui_1htconc", trans)
      spp <- tr("ui_1htspp", trans)
      grp <- tr("ui_1htgrp", trans)
      chm <- tr("ui_1htchm", trans)
      unt <- tr("ui_1htunt", trans)

      colnames(df) <- c(chm, spp, conc, grp, unt)
      df
    }) %>%
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
    }) %>%
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
    }) %>%
      bindEvent(input$handson_done, translations())

    observe({
      active_source("upload")
    }) %>%
      bindEvent(input$uploadData)

    observe({
      active_source("demo")
    }) %>%
      bindEvent(input$demoData)

    observe({
      active_source("handson")
    }) %>%
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
        # Remove any column names like X1, X2 (blank headers from excel/numbers)
        data[, colnames(data) %in% paste0("X", 1:200)] <- NULL

        # Remove completely empty columns (all NA or empty strings)
        empty_cols <- vapply(
          data,
          function(col) {
            all(is.na(col) | col == "")
          },
          logical(1)
        )
        data <- data[, !empty_cols, drop = FALSE]

        # Remove any rows with all NA or empty values
        if (ncol(data) > 0) {
          data <- data[!(rowSums(is.na(data) | data == "") == ncol(data)), ]
        }
      }
      data
    })

    names_data <- reactive({
      data <- clean_data()
      names(data) %<>% make.names()
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

      if (active_source() == "hot" && all(is.na(data[[1]]))) {
        return(FALSE)
      }

      TRUE
    })

    output$has_data <- has_data
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    output$handson <- rhandsontable::renderRHandsontable({
      x <- handson_data()
      if (!is.null(x)) {
        rhandsontable::rhandsontable(x, width = 600, useTypes = FALSE)
      }
    })

    output$viewUpload <- DT::renderDataTable({
      data <- current_data()
      req(data)

      DT::datatable(
        data,
        options = dt_options(lang()),
        class = 'table-striped table-hover table-bordered',
        selection = 'none',
        extensions = 'Buttons'
      ) %>%
        DT::formatStyle(
          columns = colnames(data),
          backgroundColor = 'white',
          border = '1px solid #ddd'
        )
    })

    return(
      list(
        data = names_data,
        clean_data = clean_data,
        data_cols = reactive({
          names(data_mod$clean_data())
        }),
        has_data = has_data
      )
    )
  })
}
