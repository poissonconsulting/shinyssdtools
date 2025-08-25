# Data Module UI
mod_data_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Demo Data Section
    span(`data-translate` = "ui_1choose", "Choose one of the following options:"),
    p(
      bslib::popover(
        bsicons::bs_icon("question-circle", style = "margin-right: 0.5rem; color: #6c757d; outline: none; border: none;"),
        div(
          span(`data-translate` = "ui_1datahelp", "This can be used to demo the app or view a dataset that 'works'."),
          br(), br(),
          "Citation:",
          tags$a("Canadian Council of Ministers of the Environment. 2009. Canadian water quality guidelines for the protection of aquatic life: Boron. In: Canadian environmental quality guidelines, 2009, Canadian Council of Ministers of the Environment, Winnipeg.", href = "http://ceqg-rcqe.ccme.ca/download/en/324/")
        ),
        placement = "right"
      ),
      span(
        span(`data-translate` = "ui_1data", "1. Use "),
        actionLink(ns("demoData"), span(`data-translate` = "ui_1data2", "boron dataset"), icon = icon("table"))
      )
    ),
    
    # CSV Upload Section  
    fileInput(ns("uploadData"),
      buttonLabel = span(tagList(icon("upload"), "csv")),
      label = span(
        bslib::popover(
          bsicons::bs_icon("question-circle", style = "margin-right: 0.5rem; color: #6c757d; outline: none; border: none;"),
          span(`data-translate` = "ui_1csvhelp", "Upload your own CSV file with concentration and species data."),
          placement = "right"
        ),
        span(`data-translate` = "ui_1csv", "2. Upload CSV file")
      ),
      placeholder = "...",
      accept = c(".csv")
    ),
    
    # Data Table Section
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        title = span(
          bslib::popover(
            bsicons::bs_icon("question-circle", style = "margin-right: 0.5rem; color: #6c757d; outline: none; border: none;"),
            span(`data-translate` = "ui_1tablehelp", "Manually enter concentration and species data in the table."),
            placement = "right"
          ),
          span(`data-translate` = "ui_1table", "3. Fill out table below:")
        ),
        value = "data_table",
        rhandsontable::rHandsontableOutput(ns("hot"))
      )
    )
  )
}

# Data Module Server
mod_data_server <- function(id, shared_values, translations) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Module-specific reactive values
    upload_values <- reactiveValues(
      upload_state = NULL,
      data_ready = FALSE  # Explicit flag to prevent early flashing
    )
    
    hot_values <- reactiveValues()
    
    # Demo data reactive
    demo_data <- reactive({
      df <- boron.data
      trans_obj <- translations()
      conc <- tr("ui_1htconc", trans_obj)
      spp <- tr("ui_1htspp", trans_obj)
      grp <- tr("ui_1htgrp", trans_obj)
      chm <- tr("ui_1htchm", trans_obj)
      unt <- tr("ui_1htunt", trans_obj)
      
      colnames(df) <- c(chm, spp, conc, grp, unt)
      df
    })
    
    # Handson table reactive
    hot_data <- reactive({
      trans_obj <- translations()
      conc <- tr("ui_1htconc", trans_obj)
      spp <- tr("ui_1htspp", trans_obj)
      grp <- tr("ui_1htgrp", trans_obj)
      
      if (!is.null(input$hot)) {
        DF <- rhandsontable::hot_to_r(input$hot)
        colnames(DF) <- c(conc, spp, grp)
        DF <- dplyr::mutate_if(DF, is.factor, as.character)
      } else {
        if (is.null(hot_values[["DF"]])) {
          DF <- data.frame(
            "Concentration" = rep(NA_real_, 10),
            "Species" = rep(NA_character_, 10),
            "Group" = rep(NA_character_, 10)
          )
        } else {
          DF <- hot_values[["DF"]]
        }
      }
      hot_values[["DF"]] <- DF
      DF
    })
    
    # Read data based on selected method
    read_data <- reactive({
      req(upload_values$upload_state)
      if (upload_values$upload_state == "upload") {
        data <- input$uploadData
        if (!grepl(".csv", data$name, fixed = TRUE)) {
          Sys.sleep(1)
          return(p("We're not sure what to do with that file type. Please upload a csv."))
        }
        return(readr::read_csv(data$datapath))
      } else if (upload_values$upload_state == "demo") {
        return(demo_data())
      } else if (upload_values$upload_state == "hot") {
        return(hot_data())
      }
    })
    
    # Clean data
    clean_data <- reactive({
      data <- read_data()
      if (length(data)) {
        # Remove any column names like X1, X2 (blank headers from excel/numbers)
        data[, colnames(data) %in% paste0("X", 1:200)] <- NULL
        # Remove any rows with all NA
        data <- data[!(rowSums(is.na(data) | data == "") == ncol(data)), ]
      }
      data
    })
    
    # Deal with unacceptable column names
    names_data <- reactive({
      data <- clean_data()
      names(data) %<>% make.names
      data
    })
    
    # Update shared values when data changes
    observe({
      data <- names_data()
      shared_values$data <- data
      shared_values$data_source <- upload_values$upload_state
      shared_values$column_names <- names(data)
    })
    
    # Render handson table
    output$hot <- rhandsontable::renderRHandsontable({
      x <- hot_data()
      if (!is.null(x)) {
        rhandsontable::rhandsontable(x, width = 600, useTypes = FALSE)
      }
    })
    
    # Event observers
    observeEvent(input$uploadData, {
      upload_values$upload_state <- "upload"
      upload_values$data_ready <- TRUE
    })
    
    observeEvent(input$demoData, {
      upload_values$upload_state <- "demo"
      upload_values$data_ready <- TRUE
    })
    
    observeEvent(input$hot, {
      upload_values$upload_state <- "hot"
      # For hot table, only set ready if there's actual data
      data <- hot_data()
      if (!is.null(data) && nrow(data) > 0 && !all(is.na(data[[1]]))) {
        upload_values$data_ready <- TRUE
      }
    })
    
    # Return reactive data for use by other modules
    return(
      list(
        data = reactive({ names_data() }),
        clean_data = reactive({ clean_data() }),
        column_names = reactive({ names(clean_data()) }),
        data_source = reactive({ upload_values$upload_state }),
        has_data = reactive({ 
          # Only show if explicitly marked as ready
          if (!upload_values$data_ready) return(FALSE)
          if (is.null(upload_values$upload_state)) return(FALSE)
          
          # Double-check data exists
          data <- tryCatch({
            names_data()
          }, error = function(e) NULL)
          
          if (is.null(data) || nrow(data) == 0) return(FALSE)
          
          TRUE
        })
      )
    )
  })
}

# Helper function for translations (will need to be accessible)
tr <- function(id, trans) {
  trans$trans[trans$id == id]
}