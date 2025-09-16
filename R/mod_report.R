# Report Module UI
mod_report_ui <- function(id) {
  ns <- NS(id)
  
  div(
    style = "padding: 1rem;",
    card(
      card_header("BCANZ Report Generation"),
      card_body(
        tagList(
          textInput(ns("toxicant"), 
                    label = span(`data-translate` = "ui_4toxname", "Toxicant name"),
                    value = ""),
          ui_download_report(ns = ns)
        )
      )
    )
  )
  
}

# Report Module Server
mod_report_server <- function(id, translations, data_mod, fit_mod, predict_mod) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Waiting screen for report generation
    waiting_screen_report <- reactive({
      trans <- translations()
      tagList(
        waiter::spin_flower(),
        tagList(
          h3(tr("ui_4gentitle", trans)),
          br(),
          h4(tr("ui_4genbody", trans))
        )
      )
    })
    
    # Parameters list for report
    params_list <- reactive({
      req(predict_mod$has_predict())
      req(fit_mod$has_fit())
      req(input$toxicant)
      
      toxicant <- input$toxicant
      data <- data_mod$data()
      dists <- fit_mod$dists()
      fit_plot <- fit_mod$fit_plot()
      fit_dist <- fit_mod$fit_dist()
      gof_table <- fit_mod$gof_table()
      model_average_plot <- predict_mod$model_average_plot()
      nboot <- as.integer(gsub("(,|\\s)", "", predict_mod$nboot()))
      
      params <- list(
        toxicant = toxicant, data = data, dists = dists,
        fit_plot = fit_plot, fit_dist = fit_dist, gof_table = gof_table,
        model_average_plot = model_average_plot, nboot = nboot
      )
      params
    })
    
    # PDF download handler
    output$reportDlPdf <- downloadHandler(
      filename = function() {
        trans <- translations()
        paste0(tr("ui_bcanz_filename", trans), ".pdf")
      },
      content = function(file) {
        waiter::waiter_show(html = waiting_screen_report(), color = "#759dbe")
        
        trans <- translations()
        temp_report <- file.path(tempdir(), tr("ui_bcanz_file", trans))
        file.copy(
          system.file(package = "shinyssdtools", file.path("extdata", tr("ui_bcanz_file", trans))),
          temp_report
        )
        params <- params_list()
        rmarkdown::render(temp_report,
          output_format = "pdf_document",
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv()),
          encoding = "utf-8"
        )
        waiter::waiter_hide()
      }
    )
    
    # HTML download handler
    output$reportDlHtml <- downloadHandler(
      filename = function() {
        trans <- translations()
        paste0(tr("ui_bcanz_filename", trans), ".html")
      },
      content = function(file) {
        waiter::waiter_show(html = waiting_screen_report(), color = "#759dbe")
        
        trans <- translations()
        temp_report <- file.path(tempdir(), tr("ui_bcanz_file", trans))
        file.copy(
          system.file(package = "shinyssdtools", file.path("extdata", tr("ui_bcanz_file", trans))),
          temp_report
        )
        params <- params_list()
        rmarkdown::render(temp_report,
          output_format = "html_document",
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv()),
          encoding = "utf-8"
        )
        waiter::waiter_hide()
      }
    )
    
    # Return reactive indicators
    return(
      list(
        can_generate_report = reactive({ 
          !is.null(shared_values$model_average_plot) && 
          !is.null(shared_values$fitted_dist) &&
          !is.null(input$toxicant) && 
          input$toxicant != ""
        })
      )
    )
  })
}