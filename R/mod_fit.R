# Fit Module UI
mod_fit_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$label(
      `for` = ns("selectConc"),
      class = "control-label",
      span(`data-translate` = "ui_2conc", "Concentration")
    ),
    selectInput(ns("selectConc"),
               label = NULL,
               choices = NULL,
               selected = NULL
    ),
    selectizeInput(ns("selectDist"),
                   label = span(`data-translate` = "ui_2dist", "Select distributions to fit"),
                   multiple = TRUE,
                   choices = c(default.dists, extra.dists),
                   selected = default.dists,
                   options = list(
                     "plugins" = list("remove_button"),
                     "create" = TRUE,
                     "persist" = FALSE
                   )
    ),
    selectInput(ns("selectUnit"),
                label = span(`data-translate` = "ui_2unit", "Select units"),
                choices = units(),
                selected = units()[1]
    ),
    checkboxInput(ns("rescale"),
      label = span(`data-translate` = "ui_2rescale", "Rescale"),
      value = FALSE
    ),
    textInput(ns("xaxis2"),
      label = span(`data-translate` = "ui_3xlab", "X-axis label"),
      value = "Concentration"
    ),
    textInput(ns("yaxis2"),
      label = span(`data-translate` = "ui_3ylab", "Y-axis label"),
      value = "Percent"
    ),
    numericInput(ns("size2"),
      label = span(`data-translate` = "ui_size", "Text size"),
      value = 12, min = 1, max = 100
    )
  )
}

# Fit Module Server
mod_fit_server <- function(id, shared_values, translations) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Waiters for async operations
    waiter_gof <- waiter::Waiter$new(id = "gofDiv", html = waiter::spin_2(), color = "white")
    waiter_distplot <- waiter::Waiter$new(id = "distPlot1", html = waiter::spin_2(), color = "white")
    
    # Reactive for column names from data module
    column_names <- reactive({
      req(shared_values$column_names)
      shared_values$column_names
    })
    
    # Guess concentration column
    guess_conc <- reactive({
      name <- column_names()
      name[grepl("conc", name %>% tolower())][1]
    })
    
    # Update concentration selectInput when data changes
    observe({
      choices <- column_names()
      selected <- guess_conc()
      updateSelectInput(session, "selectConc",
                        choices = choices,
                        selected = selected)
    })
    
    # Check fit validation
    check_fit <- reactive({
      req(input$selectConc)
      req(input$selectDist)
      req(shared_values$data)
      
      conc <- input$selectConc
      dist <- input$selectDist
      data <- shared_values$data
      trans_obj <- translations()
      
      if (length(data[[conc]]) == 0L) {
        return(tr("ui_hintdata", trans_obj))
      }
      if (!is.numeric(data[[conc]])) {
        return(tr("ui_hintnum", trans_obj))
      }
      if (any(is.na(data[[conc]]))) {
        return(tr("ui_hintmiss", trans_obj))
      }
      if (any(data[[conc]] <= 0)) {
        return(tr("ui_hintpos", trans_obj))
      }
      if (any(is.infinite(data[[conc]]))) {
        return(tr("ui_hintfin", trans_obj))
      }
      if (zero_range(data[[conc]])) {
        return(tr("ui_hintident", trans_obj))
      }
      if (length(data[[conc]]) < 6) {
        return(tr("ui_hint6", trans_obj))
      }
      if (is.null(dist)) {
        return(tr("ui_hintdist", trans_obj))
      }
      ""
    })
    
    # Fit distributions
    fit_dist <- reactive({
      req(input$selectConc)
      req(input$selectDist)
      req(check_fit() == "")
      req(shared_values$data)
      
      waiter_gof$show()
      waiter_distplot$show()
      
      data <- shared_values$data
      conc <- input$selectConc %>% make.names()
      dist <- input$selectDist
      
      x <- try(ssdtools::ssd_fit_dists(data,
        left = conc,
        dists = input$selectDist,
        silent = TRUE,
        rescale = input$rescale
      ), silent = TRUE)
      
      if (inherits(x, "try-error")) {
        x <- NULL
      }
      x
    })
    
    # Plot distributions
    plot_dist <- reactive({
      req(fit_dist())
      dist <- fit_dist()
      gp <- plot_distributions(dist,
        ylab = input$yaxis2,
        xlab = append_unit(input$xaxis2, input$selectUnit),
        text_size = input$size2
      )
      waiter_distplot$hide()
      gp
    })
    
    # Goodness of fit table
    table_gof <- reactive({
      req(fit_dist())
      
      dist <- fit_dist()
      trans_obj <- translations()
      gof <-
        ssdtools::ssd_gof(dist) %>%
        dplyr::mutate_if(is.numeric, ~ signif(., 3)) %>%
        dplyr::arrange(dplyr::desc(.data$weight))
      names(gof) <- gsub("weight", tr("ui_2weight", trans_obj), names(gof))
      waiter_gof$hide()
      gof
    })
    
    # Failed fits
    fit_fail <- reactive({
      req(input$selectDist)
      dist <- fit_dist()
      x <- paste0(setdiff(input$selectDist, names(dist)), collapse = ", ")
      x
    })
    
    # Update shared values when fit changes
    observe({
      shared_values$fitted_dist <- fit_dist()
      shared_values$selected_conc <- input$selectConc
      shared_values$selected_dists <- input$selectDist
      shared_values$selected_unit <- input$selectUnit
      shared_values$fit_plot <- plot_dist()
      shared_values$gof_table <- table_gof()
      shared_values$rescale <- input$rescale
    })
    
    # Return reactive values for use by other modules
    return(
      list(
        fitted_dist = reactive({ fit_dist() }),
        plot_dist = reactive({ plot_dist() }),
        table_gof = reactive({ table_gof() }),
        check_fit = reactive({ check_fit() }),
        fit_fail = reactive({ fit_fail() }),
        show_fit_results = reactive({ 
          !is.null(fit_dist()) && !inherits(fit_dist(), "try-error")
        })
      )
    )
  })
}