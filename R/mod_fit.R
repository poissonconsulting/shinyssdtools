# Complete Fit Module with Modern Reactive Syntax

# Fit Module UI
mod_fit_ui <- function(id) {
  ns <- NS(id)
  
  
  tagList(
    # Show full layout with sidebar when data exists
    conditionalPanel(
      condition = paste_js("has_data", ns),
      layout_sidebar(
        padding = "1rem",
        gap = "1rem",
        
        sidebar = sidebar(
          width = 350,
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
            checkboxInput(ns("rescale"),
                          label = span(`data-translate` = "ui_2rescale", "Rescale"),
                          value = FALSE
            ),
            
            # Update button with conditional icon
            div(class = "mt-3",
                actionButton(ns("updateFit"), 
                             label = tagList(
                               uiOutput(ns("update_icon")), 
                               span(`data-translate` = "ui_update_fit", "Update Fit")
                             ),
                             class = "btn-primary w-100")
            ),
            
            # Formatting controls
            hr(),
            selectInput(ns("selectUnit"),
                        label = span(`data-translate` = "ui_2unit", "Select units"),
                        choices = units(),
                        selected = units()[1]
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
        ),
        
        # Main content area when data exists
        div(
          class = "p-3",
          
          # Validation hints
          conditionalPanel(
            condition = paste_js("fit_args_fail", ns), 
            htmlOutput(ns("hintFi"))
          ),
          
          # Results when fit is successful
          conditionalPanel(
            condition = paste_js('has_fit', ns), 
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span(`data-translate` = "ui_2plot", "Plot Fitted Distributions"),
                ui_download_popover()
              ),
              card_body(
                htmlOutput(ns("fitFail")),
                plotOutput(ns("distPlot1"))
              )
            ),
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span(`data-translate` = "ui_2table", "Goodness of Fit"),
                actionButton(ns("dlFitTable"),
                             label = tagList(bsicons::bs_icon("download"), span(`data-translate` = "ui_2download", "Download")),
                             style = "padding:4px; font-size:80%"
                )
              ),
              card_body(
                div(id = ns("gofDiv"),
                    DT::dataTableOutput(ns("gofTable")))
              )
            )
          )
        )
      )
    ),
    
    conditionalPanel(
      condition =  paste0("!output['", ns("has_data"), "']"),
      ui_dashbox(span(`data-translate` = "ui_hintdata", "You have not added a dataset."))
    )
  )
}

# Fit Module Server
mod_fit_server <- function(id, translations, data_mod, main_nav) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # observe({print(main_nav())})
    
    # Track if fit tab has been visited
    fit_tab_active <- reactiveVal(FALSE)
    
    # observe({print(fit_tab_active())})
    
    # Check if user has visited the fit tab
    observe({
      if (main_nav() == "fit") {
        fit_tab_active(TRUE)
      } else {
        fit_tab_active(FALSE)
      }
    }) %>% 
      bindEvent(main_nav())
    
    # # Only allow expensive operations if tab has been visited
    # fit_tab_ready <- reactive({
    #   fit_tab_visited() && data_mod$has_data()
    # })
    
    # for conditional panel
    output$has_data <- data_mod$has_data
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)
    # clean_data <- reactive({clean_data()})
    
    waiter_gof <- waiter::Waiter$new(id = ns("gofDiv"), html = waiter::spin_2(), color = "white")
    waiter_distplot <- waiter::Waiter$new(id = ns("distPlot1"), html = waiter::spin_2(), color = "white")
    
    needs_update <- reactiveVal(FALSE)
    update_trigger <- reactiveVal(0)
    
    observe({
      needs_update(TRUE)
    }) %>%
      bindEvent(
        input$selectDist, 
        input$rescale,
        ignoreInit = TRUE
      )
    
    # Update button click - reset flag and trigger fit
    observe({
      needs_update(FALSE)
      update_trigger(update_trigger() + 1)
    }) %>%
      bindEvent(input$updateFit)
    
    # observe({
    #   if(fit_tab_active())
    #     update_trigger(update_trigger() + 1)
    # }) %>% 
    #   bindEvent(main_nav())
    
    # Auto-update for critical changes (and reset flag)
    observe({
      cat("Auto-update triggered at:", Sys.time(), "\n")
      print(input$selectConc)
      needs_update(FALSE)
      update_trigger(update_trigger() + 1)
    }) %>%
      bindEvent(input$selectConc, data_mod$data(), ignoreInit = TRUE)
    
    # Dynamic icon for update button
    output$update_icon <- renderUI({
      if (needs_update()) {
        icon("refresh", class = "text-white me-1")
      } else {
        icon("check-circle", class = "text-white me-1")
      }
    }) %>%
      bindEvent(needs_update())
    
    # Update concentration choices when data changes  
    observe({
      choices <- names(data_mod$clean_data())
      selected <- guess_conc(choices)
      if(is.na(selected))
        selected <- choices[1]
      updateSelectInput(session, "selectConc",
                        choices = choices,
                        selected = selected)
    }) %>%
      bindEvent(data_mod$clean_data())
    
    # Validation check
    iv <- InputValidator$new()
    
    # Add rules for both concentration and distribution selection
    iv$add_rule("selectConc", function(value) {
      trans <- translations()
      dat <- data_mod$data()
      
      conc_data <- dat[[value]]
      
      if (!is.numeric(conc_data)) {
        return(as.character(tr("ui_hintnum", trans)[1]))
      }
      if (any(is.na(conc_data))) {
        return(as.character(tr("ui_hintmiss", trans)[1]))
      }
      if (any(conc_data <= 0)) {
        return(as.character(tr("ui_hintpos", trans)[1]))
      }
      if (any(is.infinite(conc_data))) {
        return(as.character(tr("ui_hintfin", trans)[1]))
      }
      if (length(conc_data) < 6) {
        return(as.character(tr("ui_hint6", trans)[1]))
      }
      if (zero_range(conc_data)) {
        return(as.character(tr("ui_hintident", trans)[1]))
      }
      NULL  
    })
    
    iv$add_rule("selectDist", function(value) {
      trans <- translations()
      if (is.null(value) || length(value) == 0) {
        return(as.character(tr("ui_hintdist", trans)[1]))
      }
      NULL  
    })
    
    iv$enable()
    
    # Your existing reactive logic but simplified
    fit_args_fail <- reactive({
      !iv$is_valid()
    })
    
    output$fit_args_fail <- reactive({ fit_args_fail() })
    outputOptions(output, "fit_args_fail", suspendWhenHidden = FALSE)
    
    # Fit distributions - heavy computation, good for caching
    fit_dist <- reactive({
      # print(fit_tab_ready())
      # print(fit_tab_visited())
      print(fit_tab_active())
      
      req(update_trigger() > 0)
      req(fit_tab_active())
      
      cat("fit_dist executing at:", Sys.time(), "\n")
      
      waiter_gof$show()
      waiter_distplot$show()
      
      data <- data_mod$data()
      conc <- input$selectConc %>% make.names()
      dists <- input$selectDist
      rescale <- input$rescale
      
      x <- try(ssdtools::ssd_fit_dists(data,
                                       left = conc,
                                       dists = dists,
                                       silent = TRUE,
                                       rescale = rescale
      ), silent = TRUE)
      
      if (inherits(x, "try-error")) {
        x <- NULL
      }
      x
    }) %>%
      bindCache(
        input$selectConc,
        input$selectDist,
        input$rescale,
        data_mod$data()
      ) %>% 
      bindEvent(update_trigger(), fit_tab_active())
    
    observe({
      cat("update_trigger changed to:", update_trigger(), "at", Sys.time(), "\n")
    }) %>% bindEvent(update_trigger())
    
    # Plot - auto-updates with formatting changes
    plot_dist <- reactive({
      req(fit_dist())
      
      dist <- fit_dist()
      gp <- plot_distributions(dist,
                               ylab = input$yaxis2,
                               xlab = append_unit(input$xaxis2, input$selectUnit),
                               text_size = input$size2
      )
      gp
    }) 
      
    # Goodness of fit table
    table_gof <- reactive({
      req(fit_dist())
      
      dist <- fit_dist()
      trans <- translations()
      gof <-
        ssdtools::ssd_gof(dist) %>%
        dplyr::mutate_if(is.numeric, ~ signif(., 3)) %>%
        dplyr::arrange(dplyr::desc(.data$weight))
      names(gof) <- gsub("weight", tr("ui_2weight", trans), names(gof))
      gof
    }) 
      
    # Failed fits
    fit_fail <- reactive({
      dist <- fit_dist()
      paste0(setdiff(input$selectDist, names(dist)), collapse = ", ")
    }) %>%
      bindEvent(fit_dist())
    
    # Check if fit is valid
    has_fit <- reactive({
      !is.null(fit_dist()) && !inherits(fit_dist(), "try-error")
    }) %>%
      bindEvent(fit_dist())
    
    output$has_fit <- reactive({ has_fit() })
    outputOptions(output, "has_fit", suspendWhenHidden = FALSE)
    
    # Render status tracking for waiters
    render_status <- reactiveValues(plot_ready = FALSE, table_ready = FALSE)
    
    # Reset render status when new fitting starts
    observe({
      render_status$plot_ready <- FALSE
      render_status$table_ready <- FALSE
    }) %>%
      bindEvent(fit_dist())
    
    # Render outputs
    output$distPlot1 <- renderPlot({
      result <- plot_dist()
      render_status$plot_ready <- TRUE
      result
    }) 
    
    output$gofTable <- DT::renderDataTable({
      result <- DT::datatable(
        table_gof(),
        options = list(
          dom = "t",
          processing = FALSE,
          autoWidth = FALSE,
          deferRender = TRUE
        )
      )
      # required as datatable render can be slow
      render_status$table_ready <- TRUE
      result
    }) 
    
    # Hide waiters when both renders complete
    observe({
      if (render_status$plot_ready && render_status$table_ready) {
        waiter_distplot$hide()
        waiter_gof$hide()
      }
    }) %>%
      bindEvent(render_status$plot_ready, render_status$table_ready)
    
    # Replace your current fitFail output with this observer:
    observe({
      failed <- fit_fail()
      req(failed != "")
      print(failed)
      
      failed_dists <- strsplit(failed, ", ")[[1]]
      message <- paste(failed_dists, tr("ui_hintfail", translations()))
      print(message)
      
      shinytoastr::toastr_warning(
        message = message,
        title = "Distribution Fitting",
        timeOut = 8000,  # 8 seconds before auto-dismiss
        closeButton = TRUE
      )
    }) %>%
      bindEvent(fit_fail())
    
    output$fitFail <- renderText({
      failed <- fit_fail()
      req(failed != "")
      HTML(paste0("<font color='grey'>", paste(
        failed, tr("ui_hintfail", translations())
      ), "</font>"))
    }) %>%
      bindEvent(fit_fail())
    
    # Download handlers
    output$fitDlPlot <- downloadHandler(
      filename = function() {
        "ssdtools_distFitPlot.png"
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot_dist(), 
                        device = "png",
                        width = get_width2(), 
                        height = get_height2(), 
                        dpi = get_dpi2()
        )
      }
    )
    
    output$fitDlRds <- downloadHandler(
      filename = function() {
        "ssdtools_distFitPlot.rds"
      },
      content = function(file) {
        saveRDS(plot_dist(), file = file)
      }
    )
    
    output$fitDlTableHidden <- downloadHandler(
      filename = function() {
        "ssdtools_distGofTable.csv"
      },
      content = function(file) {
        readr::write_csv(table_gof() %>% dplyr::as_tibble(), file)
      }
    )
    
    observe({
      shinyjs::click("fitDlTableHidden")
    }) %>%
      bindEvent(input$dlFitTable)
    
    # Return reactive values for use by other modules
    return(
      list(
        fit_dist = fit_dist,
        has_fit = has_fit
      )
    )
  })
}