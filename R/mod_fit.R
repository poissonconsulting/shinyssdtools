# Complete Fit Module with Modern Reactive Syntax

# Fit Module UI
mod_fit_ui <- function(id) {
  ns <- NS(id)
  
  
  tagList(
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
            
            div(class = "mt-3",
                actionButton(ns("updateFit"), 
                             label = tagList(
                               uiOutput(ns("update_icon")), 
                               span(`data-translate` = "ui_update_fit", "Update Fit")
                             ),
                             class = "btn-primary w-100")
            ),
            
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
        
        div(
          class = "p-3",
          conditionalPanel(
            condition = paste_js('has_fit', ns), 
            div(id = ns("divPlotDist"),
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span(`data-translate` = "ui_2plot", "Plot Fitted Distributions"),
                ui_download_popover(ns = ns)
              ),
              card_body(
                htmlOutput(ns("fitFail")),
                plotOutput(ns("plotDist"))
              )
            )),
            div(id = ns("divGof"),
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span(`data-translate` = "ui_2table", "Goodness of Fit"),
                ui_download_popover_table(ns = ns)
              ),
              card_body(
                
                    DT::dataTableOutput(ns("tableGof")))
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

mod_fit_server <- function(id, translations, data_mod) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$has_data <- data_mod$has_data
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)
    
    waiter_gof <- waiter::Waiter$new(id = ns("divGof"), html = waiter::spin_2(), color = "white")
    waiter_distplot <- waiter::Waiter$new(id = ns("divPlotDist"), html = waiter::spin_2(), color = "white")
    
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
    
    # Manual update with button 
    observe({
      needs_update(FALSE)
      update_trigger(update_trigger() + 1)
    }) %>%
      bindEvent(input$updateFit)
    
    # Auto-update for critical changes 
    observe({
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
    

# validation --------------------------------------------------------------
    iv <- InputValidator$new()
    
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
    

# fit reactives and outputs -----------------------------------------------
    fit_dist <- reactive({
      req(update_trigger() > 0)

      waiter_gof$show()
      waiter_distplot$show()
      
      data <- data_mod$data()
      conc <- make.names(input$selectConc) 
      dists <- input$selectDist
      rescale <- input$rescale
      
      safely_try(ssdtools::ssd_fit_dists(data,
                                         left = conc,
                                         dists = dists,
                                         silent = TRUE,
                                         rescale = rescale
      ))
    }) %>%
      bindCache(
        input$selectConc,
        input$selectDist,
        input$rescale,
        data_mod$data()
      ) %>% 
      bindEvent(update_trigger())
    
    plot_dist <- reactive({
      dist <- fit_dist()
      req(dist)
      
      plot_distributions(dist,
                               ylab = input$yaxis2,
                               xlab = append_unit(input$xaxis2, input$selectUnit),
                               text_size = input$size2
      )
    }) 
      
    table_gof <- reactive({
      dist <- fit_dist()
      req(dist)
      
      trans <- translations()
      gof <-
        ssdtools::ssd_gof(dist) %>%
        dplyr::mutate_if(is.numeric, ~ signif(., 3)) %>%
        dplyr::arrange(dplyr::desc(.data$weight))
      names(gof) <- gsub("weight", tr("ui_2weight", trans), names(gof))
      gof
    }) 
    
    # render plot and table - waiter stops when plot and table ready
    render_status <- reactiveValues(plot_ready = FALSE, table_ready = FALSE)
    
    observe({
      render_status$plot_ready <- FALSE
      render_status$table_ready <- FALSE
    }) %>%
      bindEvent(fit_dist())
    
    output$plotDist <- renderPlot({
      result <- plot_dist()
      render_status$plot_ready <- TRUE
      result
    }) 
    
    output$tableGof <- DT::renderDataTable({
      result <- DT::datatable(
        table_gof(),
        options = list(
          dom = "t",
          processing = FALSE,
          autoWidth = FALSE,
          deferRender = TRUE
        )
      )
      render_status$table_ready <- TRUE
      result
    }) 
    
    observe({
      if (render_status$plot_ready && render_status$table_ready) {
        waiter_distplot$hide()
        waiter_gof$hide()
      }
    }) %>%
      bindEvent(render_status$plot_ready, render_status$table_ready)
    
    # Notify when failed fits
    fit_fail <- reactive({
      dist <- fit_dist()
      paste0(setdiff(input$selectDist, names(dist)), collapse = ", ")
    }) %>%
      bindEvent(fit_dist())
    
    output$fitFail <- renderText({
      failed <- fit_fail()
      req(failed != "")
      HTML(paste0("<font color='grey'>", paste(
        failed, tr("ui_hintfail", translations())
      ), "</font>"))
    }) %>%
      bindEvent(fit_fail())
    
# download handlers -------------------------------------------------------
    output$fitDlPlot <- downloadHandler(
      filename = function() {
        "ssdtools_distFitPlot.png"
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot_dist(), 
                        device = "png",
                        width = input$width2, 
                        height = input$height2, 
                        dpi = input$dpi2
        )
      }
    )
    
    output$fitDlRds <- downloadHandler(
      filename = function() {
        "ssdtools_fit_plot.rds"
      },
      content = function(file) {
        saveRDS(plot_dist(), file = file)
      }
    )
    
    output$fitDlCsv <- downloadHandler(
      filename = function() {
        "ssdtools_gof_table.csv"
      },
      content = function(file) {
        readr::write_csv(dplyr::as_tibble(table_gof()), file)
      }
    )
    
    output$fitDlXlsx <- downloadHandler(
      filename = function() {
        "ssdtools_gof_table.xlsx"
      },
      content = function(file) {
        writexl::write_xlsx(dplyr::as_tibble(table_gof()), file)
      }
    )
    
    # observe({
    #   shinyjs::click("fitDlTableHidden")
    # }) %>%
    #   bindEvent(input$dlFitTable)
    

# return values ------------------------------------------------------------
    has_fit <- reactive({
      !is.null(fit_dist()) && !inherits(fit_dist(), "try-error")
    }) %>%
      bindEvent(fit_dist())
    
    output$has_fit <- has_fit
    outputOptions(output, "has_fit", suspendWhenHidden = FALSE)
    
    return(
      list(
        fit_dist = fit_dist,
        conc_column = reactive({input$selectConc}),
        units = reactive({input$selectUnit}),
        has_fit = has_fit
      )
    )
  })
}