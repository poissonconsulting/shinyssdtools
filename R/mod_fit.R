# Fit Module UI
mod_fit_ui <- function(id) {
  ns <- NS(id)
  
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
  )),
conditionalPanel(
  condition = "input.main_nav == 'fit'",
  div(
    class = "p-3",
    conditionalPanel(condition = paste_js('fit_args_fail', ns), htmlOutput("hintFi")),
    conditionalPanel(condition = paste_js('has_fit', ns), uiOutput("ui_2plot")),
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
          # Hidden download button for programmatic triggering
          # downloadButton("fitDlTableHidden", "", style = "display: none;")
        )
      )
    ))))
}

# Fit Module Server
mod_fit_server <- function(id, translations, data_mod) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Waiters for async operations
    waiter_gof <- waiter::Waiter$new(id = ns("gofDiv"), html = waiter::spin_2(), color = "white")
    waiter_distplot <- waiter::Waiter$new(id = ns("distPlot1"), html = waiter::spin_2(), color = "white")
    
    # --- render column choices
    # Update concentration selectInput when data changes
    observe({
      choices <- names(data_mod$clean_data())
      selected <- guess_conc(choices)
      updateSelectInput(session, "selectConc",
                        choices = choices,
                        selected = selected)
    }) %>% 
      bindEvent(data_mod$clean_data())
    
    # observe({
    #   print(check_fit())
    # })
    
    # Check fit validation
    check_fit_args <- reactive({
      req(input$selectConc)
      req(input$selectDist)
      req(data_mod$data())
      
      conc <- input$selectConc
      dist <- input$selectDist
      data <- data_mod$data()
      trans <- translations()
      
      if (length(data[[conc]]) == 0L) {
        return(tr("ui_hintdata", trans))
      }
      if (!is.numeric(data[[conc]])) {
        return(tr("ui_hintnum", trans))
      }
      if (any(is.na(data[[conc]]))) {
        return(tr("ui_hintmiss", trans))
      }
      if (any(data[[conc]] <= 0)) {
        return(tr("ui_hintpos", trans))
      }
      if (any(is.infinite(data[[conc]]))) {
        return(tr("ui_hintfin", trans))
      }
      if (zero_range(data[[conc]])) {
        return(tr("ui_hintident", trans))
      }
      if (length(data[[conc]]) < 6) {
        return(tr("ui_hint6", trans))
      }
      if (is.null(dist)) {
        return(tr("ui_hintdist", trans))
      }
      ""
    })
    
    # Fit distributions
    fit_dist <- reactive({
      # req(input$selectConc)
      # req(input$selectDist)
      print("hi")
      req(check_fit_args() == "")
      print("hi2")
      # req(data_mod$data())
      
      waiter_gof$show()
      waiter_distplot$show()
      
      data <- data_mod$data()
      conc <- input$selectConc %>% make.names()
      dist <- input$selectDist
      
      x <- try(ssdtools::ssd_fit_dists(data,
        left = conc,
        dists = dist,
        silent = TRUE,
        rescale = input$rescale
      ), silent = TRUE)
      
      if (inherits(x, "try-error")) {
        x <- NULL
      }
      print("hi2")
      print(x)
      x
    }) 
    
    # Plot distributions
    plot_dist <- reactive({
      dist <- fit_dist()
      gp <- plot_distributions(dist,
        ylab = input$yaxis2,
        xlab = append_unit(input$xaxis2, input$selectUnit),
        text_size = input$size2
      )
      gp
    }) %>% 
      bindEvent(fit_dist())
    
    # Goodness of fit table
    table_gof <- reactive({
      dist <- fit_dist()
      trans <- translations()
      gof <-
        ssdtools::ssd_gof(dist) %>%
        dplyr::mutate_if(is.numeric, ~ signif(., 3)) %>%
        dplyr::arrange(dplyr::desc(.data$weight))
      names(gof) <- gsub("weight", tr("ui_2weight", trans), names(gof))
      gof
    }) %>% 
      bindEvent(fit_dist())
    
    # Failed fits
    fit_fail <- reactive({
      dist <- fit_dist()
      x <- paste0(setdiff(input$selectDist, names(dist)), collapse = ", ")
      x
    }) %>% 
      bindEvent(fit_dist())
    
    # --- render fit results ----
    # render_status <- reactiveValues(plot_ready = FALSE, table_ready = FALSE)
    # observe({
    #   render_status$plot_ready <- FALSE
    #   render_status$table_ready <- FALSE
    # }) %>%
    #   bindEvent(fit_dist())
    
    output$distPlot1 <- renderPlot({
      result <- plot_dist()
      waiter_distplot$hide()
      result
    }) %>% 
      bindEvent(plot_dist())
    
    output$gofTable <- DT::renderDataTable({
      result <- DT::datatable(table_gof(), options = list(dom = "t", 
                                                          processing = FALSE, 
                                                          # autoWidth = FALSE,
                                                          deferRender = TRUE))
      waiter_gof$hide()
      result
    }) %>% 
      bindEvent(table_gof())
    
    # observe({
    #   if (render_status$plot_ready && render_status$table_ready) {
    #     
    #     
    #   }
    # }) %>%
    #   bindEvent(render_status$plot_ready, render_status$table_ready)
    # 
    output$fitFail <- renderText({
      failed <- fit_fail()
      req(failed != "")
      HTML(paste0("<font color='grey'>", paste(
        failed_fits, tr("ui_hintfail", trans())
      ), "</font>"))
    })
    
    has_fit <- reactive({
       !is.null(fit_dist()) && 
        !inherits(fit_dist(), "try-error")
    })
    output$has_fit <- has_fit
    outputOptions(output, "has_fit", suspendWhenHidden = FALSE)
    
    fit_args_fail <- reactive({
      check_fit_args() != ""
    })
    output$fit_args_fail <- fit_args_fail
    outputOptions(output, "fit_args_fail", suspendWhenHidden = FALSE)
    
    output$hintFi <- renderText(hint(check_fit_args()))
    
    # # Update shared values when fit changes
    # observe({
    #   shared_values$fitted_dist <- fit_dist()
    #   shared_values$selected_conc <- input$selectConc
    #   shared_values$selected_dists <- input$selectDist
    #   shared_values$selected_unit <- input$selectUnit
    #   shared_values$fit_plot <- plot_dist()
    #   shared_values$gof_table <- table_gof()
    #   shared_values$rescale <- input$rescale
    # })
    

# downloads ---------------------------------------------------------------
    output$fitDlPlot <- downloadHandler(
      filename = function() {
        "ssdtools_distFitPlot.png"
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot_dist(), device = "png",
                        width = get_width2(), height = get_height2(), dpi = get_dpi2()
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
      content <- function(file) {
        readr::write_csv(table_gof() %>% dplyr::as_tibble(), file)
      }
    )
    
    observeEvent(input$dlFitTable, {
      # Trigger the hidden download button
      shinyjs::click("fitDlTableHidden")
    })    
    
# return ------------------------------------------------------------------
    return(
      list(
        fit_dist = fit_dist,
        # plot_dist = plot_dist,
        # table_gof = table_gof,
        # check_fit = check_fit,
        # fit_fail = fit_fail,
        has_fit = has_fit
      )
    )
  })
}