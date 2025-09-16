# Report Module UI
mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = paste_js('has_predict', ns = ns),
  layout_sidebar(
    padding = "1rem",
    gap = "1rem",
    sidebar = sidebar(
      width = 375,
      style = "height: calc(100vh - 150px); overflow-y: auto; overflow-x: hidden;",
      tagList(
        textInput(ns("toxicant"), 
                  label = span(`data-translate` = "ui_4toxname", "Toxicant name"),
                  value = ""),
        uiOutput(ns("uiBootSamp")),
        actionButton(ns("generateReport"), 
                     label = tagList(
                       bsicons::bs_icon("file-earmark-text"), 
                       span(`data-translate` = "ui_getreport", "Get Report")
                     ),
                     class = "btn-primary w-100")
      )
    ),
    div(
      class = "p-3",
      conditionalPanel(
        condition = paste_js("has_preview", ns),
        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span(`data-translate` = "ui_prevreport", "Preview report")
          ),
          card_body(padding = 25,
            ui_download_report(ns = ns),
            tags$iframe(
              src = "",
              id = ns("pdfPreview"),
              style = "width: 100%; height: 600px; border: 1px solid #ddd; border-radius: 4px;"
            )
          )
        )
      )
    )
  )),
  conditionalPanel(
    condition =  paste0("!output['", ns("has_predict"), "']"),
    ui_dashbox(span(`data-translate` = "ui_hintpredict", "You have not successfully generated predictions yet. Run the 'Predict' tab first."))
  ))
}

# Report Module Server
mod_report_server <- function(id, translations, lang, data_mod, fit_mod, predict_mod) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$has_predict <- predict_mod$has_predict
    outputOptions(output, "has_predict", suspendWhenHidden = FALSE)
    
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
    
    pred_cl <- reactive({
      fit <- fit_mod$fit_dist()
      req(fit)
      nboot <- clean_nboot(predict_mod$nboot())
      print(nboot)
      avehc <- ssd_hc(fit, proportion = c(0.01, 0.05, 0.1, 0.2), ci = TRUE, 
                      nboot = nboot, min_pboot = 0.8)
      avehc |>
        dplyr::mutate(HCx = proportion*100,
               PCx = (1 - proportion) * 100) |> 
        dplyr::select(HCx, PCx, est, se, lcl, ucl, nboot, pboot)
    })
    
    output$uiBootSamp <- renderUI({
      current <- lang()
      choices <- c("500", "1,000", "5,000", "10,000")
      if(current == "french"){
        choices <- c("500", "1 000", "5 000", "10 000")
      }
      selectInput(
        ns("bootSamp"),
        label = div(
          span(`data-translate` = "ui_3samples", "Bootstrap samples"),
          bslib::tooltip(
            bsicons::bs_icon("question-circle", style = "margin-left: 0.5rem; color: #6c757d; outline: none; border: none;"),
            span(`data-translate` = "ui_3bshint", "10,000 bootstrap samples recommended"),
            placement = "right"
          )
        ),
        
        choices = choices,
        selected = predict_mod$nboot()
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
      gof_table <- fit_mod$gof_table()
      model_average_plot <- predict_mod$model_average_plot()
      pred <- pred_cl()
      
      params <- list(
        toxicant = toxicant, data = data, dists = dists,
        fit_plot = fit_plot, gof_table = gof_table,
        model_average_plot = model_average_plot, pred_cl = pred
      )
      params
    })
    
    # Generate PDF report for preview
    report_preview_pdf <- reactive({
      waiter::waiter_show(html = waiting_screen_report(), color = "#759dbe")
      
      trans <- translations()
      temp_report <- file.path(tempdir(), tr("ui_bcanz_file", trans))
      file.copy(
        system.file(package = "shinyssdtools", file.path("extdata", tr("ui_bcanz_file", trans))),
        temp_report
      )
      
      temp_pdf <- tempfile(fileext = ".pdf")
      params <- params_list()
      print(params)
      
      rmarkdown::render(temp_report,
        output_format = "pdf_document",
        output_file = temp_pdf,
        params = params,
        envir = new.env(parent = globalenv()),
        encoding = "utf-8"
      )
      
      waiter::waiter_hide()
      temp_pdf
    }) %>%
      bindEvent(input$generateReport)
    
    has_preview <- reactive({
      !is.null(report_preview_pdf()) && file.exists(report_preview_pdf())
    }) %>%
      bindEvent(report_preview_pdf())
    
    output$has_preview <- has_preview
    outputOptions(output, "has_preview", suspendWhenHidden = FALSE)
    
    observe({
      pdf_path <- report_preview_pdf()
      if (!is.null(pdf_path) && file.exists(pdf_path)) {
        www_dir <- file.path("inst", "app", "www")
        if (!dir.exists(www_dir)) {
          www_dir <- file.path("www")
        }
        if (!dir.exists(www_dir)) {
          dir.create(www_dir, recursive = TRUE)
        }
        
        preview_file <- file.path(www_dir, "report_preview.pdf")
        file.copy(pdf_path, preview_file, overwrite = TRUE)
        
        # Update iframe src
        shinyjs::runjs(paste0("
          document.getElementById('", ns("pdfPreview"), "').src = 'report_preview.pdf';
        "))
      }
    }) %>%
      bindEvent(report_preview_pdf())
    
    # reuse generated pdf
    output$reportDlPdf <- downloadHandler(
      filename = function() {
        trans <- translations()
        paste0(tr("ui_bcanz_filename", trans), ".pdf")
      },
      content = function(file) {
        file.copy(report_preview_pdf(), file)
      }
    )
    
    # always generate HTML file 
    output$reportDlHtml <- downloadHandler(
      filename = function() {
        trans <- translations()
        paste0(tr("ui_bcanz_filename", trans), ".html")
      },
      content = function(file) {
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
      }
    )
  })
}