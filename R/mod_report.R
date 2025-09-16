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
mod_report_server <- function(id, shared_values, translations) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Waiting screen for report generation
    waiting_screen_report <- reactive({
      trans_obj <- translations()
      tagList(
        waiter::spin_flower(),
        tagList(
          h3(tr("ui_4gentitle", trans_obj)),
          br(),
          h4(tr("ui_4genbody", trans_obj))
        )
      )
    })
    
    # Parameters list for report
    params_list <- reactive({
      req(shared_values$model_average_plot)
      req(input$toxicant)
      
      toxicant <- input$toxicant
      data <- shared_values$data
      dists <- shared_values$selected_dists
      fit_plot <- shared_values$fit_plot
      fit_dist <- shared_values$fitted_dist
      gof_table <- shared_values$gof_table
      model_average_plot <- shared_values$model_average_plot
      
      # Get bootstrap samples if available from predict module
      nboot <- if (!is.null(shared_values$bootstrap_samples)) {
        as.integer(gsub("(,|\\s)", "", shared_values$bootstrap_samples))
      } else {
        1000L
      }
      
      params <- list(
        toxicant = toxicant, 
        data = data, 
        dists = dists,
        fit_plot = fit_plot, 
        fit_dist = fit_dist, 
        gof_table = gof_table,
        model_average_plot = model_average_plot, 
        nboot = nboot
      )
      params
    })
    
    # PDF download handler
    output$dl_pdf <- downloadHandler(
      filename = function() {
        trans_obj <- translations()
        paste0(tr("ui_bcanz_filename", trans_obj), ".pdf")
      },
      content = function(file) {
        waiter::waiter_show(html = waiting_screen_report(), color = "rgba(44,62,80, 1)")
        
        trans_obj <- translations()
        temp_report <- file.path(tempdir(), tr("ui_bcanz_file", trans_obj))
        file.copy(
          system.file(package = "shinyssdtools", file.path("extdata", tr("ui_bcanz_file", trans_obj))),
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
    output$dl_html <- downloadHandler(
      filename = function() {
        trans_obj <- translations()
        paste0(tr("ui_bcanz_filename", trans_obj), ".html")
      },
      content = function(file) {
        waiter::waiter_show(html = waiting_screen_report(), color = "rgba(44,62,80, 1)")
        
        trans_obj <- translations()
        temp_report <- file.path(tempdir(), tr("ui_bcanz_file", trans_obj))
        file.copy(
          system.file(package = "shinyssdtools", file.path("extdata", tr("ui_bcanz_file", trans_obj))),
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
    
    waiting_screen_report <- reactive({
      tagList(
        waiter::spin_flower(),
        tagList(
          h3(tr("ui_4gentitle", trans())),
          br(),
          h4(tr("ui_4genbody", trans()))
        )
      )
    })
    
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