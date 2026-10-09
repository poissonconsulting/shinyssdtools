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

# Export Module UI
#
# The last step: the BCANZ report, every plot and table as a file, and the R
# script that reproduces the analysis (`rcode`, from mod_rcode_ui()).

# The files of the Download panel: the output of each format's download
# handler (in mod_export_server()) and the output that shows the row once
# its result exists.
export_files <- list(
  data = list(icon = "database", key = "ui_data", text = "Data", formats = c(CSV = "dataDlCsv", XLSX = "dataDlXlsx"), when = "has_data"),
  fit_plot = list(icon = "file-image", key = "ui_2plot", text = "Plot fitted distributions", formats = c(PNG = "fitDlPlot", RDS = "fitDlRds"), when = "has_fit"),
  gof = list(icon = "file-spreadsheet", key = "ui_2table", text = "Goodness of Fit table", formats = c(CSV = "fitDlCsv", XLSX = "fitDlXlsx"), when = "has_fit"),
  pred_plot = list(icon = "file-image", key = "ui_3model", text = "Plot model average and estimate hazard concentration", formats = c(PNG = "predDlPlot", RDS = "predDlRds"), when = "has_predict"),
  cl = list(icon = "file-spreadsheet", key = "ui_3cl2", text = "Confidence limits", formats = c(CSV = "predDlCsv", XLSX = "predDlXlsx"), when = "has_cl")
)

mod_export_ui <- function(id, rcode = NULL) {
  ns <- NS(id)

  png_settings <- layout_column_wrap(
    width = 1 / 3,
    gap = "0.75rem",
    numericInput(ns("width"), span(`data-translate` = "ui_2width", "Width"), value = 6, min = 1, max = 50, step = 1),
    numericInput(ns("height"), span(`data-translate` = "ui_2height", "Height"), value = 4, min = 1, max = 50, step = 1),
    numericInput(ns("dpi"), span(`data-translate` = "ui_2dpi", "Dpi"), value = 300, min = 50, max = 2000, step = 50)
  )

  file_row <- function(file) {
    conditionalPanel(
      condition = paste_js(file$when, ns),
      div(
        class = "d-flex align-items-center gap-3 border rounded-3 p-3",
        div(class = "ssd-tile-icon bg-primary-subtle text-primary-emphasis", lucide(file$icon)),
        div(class = "flex-grow-1 fw-medium", span(`data-translate` = file$key, file$text)),
        div(
          class = "d-flex gap-2 flex-shrink-0",
          lapply(names(file$formats), function(format) {
            button(ns(file$formats[[format]]), format, icon = "download", variant = "outline", size = "sm", download = TRUE)
          })
        )
      )
    )
  }

  # The BCANZ report: its settings, and the actions its state allows: Get
  # Report while its bootstrap is needed, a spinner and Cancel while it
  # renders, and once it is current, Preview, its files and a ZIP of every
  # BCANZ output.
  report_actions <- tagList(
    conditionalPanel(
      condition = sprintf("!%s && %s", paste_js("report_running", ns), paste_js("needs_bootstrap", ns)),
      button(
        ns("generateReport"),
        span(`data-translate` = "ui_getreport", "Get Report"),
        icon = "file-text",
        variant = "soft",
        size = "sm"
      )
    ),
    conditionalPanel(
      condition = paste_js("report_running", ns),
      div(
        class = "d-flex align-items-center gap-2 small",
        busy_icon(),
        span(`data-translate` = "ui_4gentitle", "Generating report..."),
        button(ns("cancelReport"), span(`data-translate` = "ui_cancel", "Cancel"), icon = "x", variant = "outline", size = "sm")
      )
    ),
    conditionalPanel(
      condition = sprintf("%s && !%s", paste_js("has_preview", ns), paste_js("report_running", ns)),
      div(
        class = "d-flex flex-wrap gap-2 justify-content-end",
        button(ns("previewReport"), span(`data-translate` = "ui_prevreport", "Preview report"), icon = "book-open", variant = "ghost", size = "sm"),
        button(ns("reportDlPdf"), "PDF", icon = "download", variant = "outline", size = "sm", download = TRUE),
        button(ns("reportDlHtml"), "HTML", icon = "download", variant = "outline", size = "sm", download = TRUE),
        button(
          ns("bcanzZip"), "ZIP", icon = "file-archive", variant = "outline", size = "sm", download = TRUE,
          title = "The report and every BCANZ output"
        )
      )
    )
  )

  report_row <- div(
    class = "border rounded-3 p-3",
    div(
      class = "d-flex flex-wrap align-items-center gap-3",
      div(class = "ssd-tile-icon bg-primary-subtle text-primary-emphasis", lucide("file-text")),
      div(
        class = "flex-grow-1",
        div(
          class = "fw-medium ssd-card-title",
          span(`data-translate` = "ui_tabreport", "BCANZ report") |>
            shinyhelper::helper(type = "markdown", content = "reportTab", size = "l", colour = color_primary, buttonLabel = "OK")
        ),
        div(class = "small text-body-secondary", htmlOutput(ns("describeReport"), inline = TRUE))
      ),
      div(class = "flex-shrink-0", report_actions)
    )
  )

  downloads <- panel(
    span(`data-translate` = "ui_2download", "Download"),
    div(class = "d-flex flex-column gap-2", report_row, lapply(export_files, file_row)),
    div(
      class = "border-top pt-3",
      div(class = "ssd-eyebrow text-body-secondary mb-2", span(`data-translate` = "ui_2png", "PNG file formatting options")),
      png_settings
    )
  )

  page <- function(value, icon, translate_key, default_text, content) {
    nav_panel(
      title = span(
        class = "d-inline-flex align-items-center gap-2",
        lucide(icon),
        span(`data-translate` = translate_key, default_text)
      ),
      value = value,
      content
    )
  }

  tagList(
    conditionalPanel(
      condition = paste_js("has_predict", ns),
      page_header(
        span(`data-translate` = "ui_export", "Export"),
        button(ns("downloadAll"), span(`data-translate` = "ui_download_all", "Download all"), icon = "file-archive", download = TRUE)
      ),
      # The pages of the step, picked in its sidebar as on the Help tab.
      navset_pill_list(
        id = ns("page"),
        well = FALSE,
        widths = c(3, 9),
        page("downloads", "download", "ui_2download", "Download", downloads),
        page("rcode", "code", "ui_tabcode", "Get R code", rcode)
      )
    ),
    conditionalPanel(
      condition = sprintf("!%s", paste_js("has_predict", ns)),
      page_header(span(`data-translate` = "ui_export", "Export")),
      empty_state(
        "calculator",
        span(
          `data-translate` = "ui_hintpredict",
          "You have not successfully generated predictions yet. Run the 'Predict' tab first."
        ),
        action = step_button(ns("goPredict"), "ui_goto_predict", "Go to Predict", variant = "outline")
      )
    )
  )
}

# Export Module Server
mod_export_server <- function(
  id,
  translations,
  lang,
  data_mod,
  fit_mod,
  predict_mod,
  main_nav = reactive("export"),
  code = function() ""
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$has_predict <- predict_mod$has_predict
    outputOptions(output, "has_predict", suspendWhenHidden = FALSE)

    # The report's parameters other than its confidence limits, which the
    # report job adds.
    params_list <- reactive({
      req(predict_mod$has_predict())
      req(fit_mod$has_fit())

      list(
        toxicant = data_mod$toxicant_name(),
        data = data_mod$clean_data(),
        dists = fit_mod$dists(),
        fit_plot = fit_mod$fit_plot(),
        gof_table = fit_mod$gof_table(),
        model_average_plot = predict_mod$model_average_plot()
      )
    })

    # The report renders on a mirai daemon (task_runner()). Once the
    # model-averaged curve of the fit has been bootstrapped with the report's
    # number of samples (by Get CL or Get Report; predict_mod$curve_lookup()),
    # the report renders by itself while the Report step is open, and again
    # when its inputs change. Otherwise Get Report bootstraps the curve first.
    report_runner <- task_runner()
    report_request <- reactiveVal(NULL)
    report_result <- reactiveVal(NULL)

    # The report uses the Predict step's bootstrap samples, so it reuses the
    # confidence limits computed there.
    report_nboot <- reactive(clean_nboot(req(predict_mod$nboot())))

    report_inputs <- reactive({
      list(
        fit = req(fit_mod$fit_dist()),
        nboot = report_nboot(),
        params = params_list(),
        template = tr("ui_bcanz_file", translations())
      )
    })

    report_curve <- reactive({
      predict_mod$curve_lookup(req(fit_mod$fit_dist()), report_nboot())
    })

    render <- function(inputs, pred) {
      report_request(c(inputs, list(bootstrap = is.null(pred))))
      report_runner$invoke(
        report_job,
        list(
          fit = inputs$fit,
          nboot = inputs$nboot,
          pred = pred,
          params = inputs$params,
          template = inputs$template
        )
      )
    }

    observe({
      inputs <- report_inputs()
      render(inputs, predict_mod$curve_lookup(inputs$fit, inputs$nboot))
    }) |>
      bindEvent(input$generateReport)

    # Inputs that change together, such as a toxicant name as it is typed,
    # render once.
    settled_inputs <- debounce(report_inputs, 800)

    observe({
      req(main_nav() == "export")
      inputs <- settled_inputs()
      pred <- predict_mod$curve_lookup(inputs$fit, inputs$nboot)
      request <- isolate(report_request())
      already <- !is.null(request) &&
        identical(inputs, request[names(inputs)]) &&
        (isolate(report_runner$running()) || !is.null(isolate(report_result())))
      if (!is.null(pred) && !already) render(inputs, pred)
    })

    observe(report_runner$cancel()) |>
      bindEvent(input$cancelReport)

    observe({
      done <- report_runner$done()
      if (!is.null(done$error)) {
        showNotification(
          div(
            role = "alert",
            div(class = "fw-semibold", tr("ui_report_failed", translations())),
            div(done$error)
          ),
          type = "error",
          duration = 10
        )
        return()
      }
      request <- report_request()
      predict_mod$curve_store(request$fit, request$nboot, done$value$pred)
      report_result(c(done$value, request[c("fit", "nboot", "params", "template")]))
    }) |>
      bindEvent(report_runner$done())

    # The report while it is of the current fit and number of samples; the
    # downloads are of this report, so they always match the preview.
    current_report <- reactive({
      report <- report_result()
      fit <- fit_mod$fit_dist()
      if (!is.null(report) && identical(report$fit, fit) &&
        identical(report$nboot, report_nboot())) {
        report
      }
    })

    output$report_running <- reactive(report_runner$running())
    outputOptions(output, "report_running", suspendWhenHidden = FALSE)
    output$needs_bootstrap <- reactive(is.null(report_curve()))
    outputOptions(output, "needs_bootstrap", suspendWhenHidden = FALSE)

    output$describeReport <- renderText({
      trans <- translations()
      samples <- paste0(tr("ui_3samples", trans), ": ", predict_mod$nboot())
      if (!is.null(report_curve())) {
        return(samples)
      }
      HTML(paste0(
        samples, ". ",
        tr("ui_3cldesc3", trans), " ",
        estimate_time(report_nboot(), lang()), " ",
        tr("ui_3cldesc4", trans)
      ))
    })

    # Links open in a new tab rather than within the preview's iframe.
    report_preview_html <- reactive({
      html <- req(current_report())$html
      gsub("<a href=", "<a target=\"_blank\" href=", html, fixed = TRUE)
    })

    has_preview <- reactive(!is.null(current_report()))

    output$has_preview <- has_preview
    outputOptions(output, "has_preview", suspendWhenHidden = FALSE)

    observe({
      showModal(modalDialog(
        tags$iframe(
          srcdoc = report_preview_html(),
          class = "ssd-report-frame",
          title = "BCANZ report",
          sandbox = "allow-same-origin allow-scripts allow-popups allow-popups-to-escape-sandbox"
        ),
        title = tr("ui_prevreport", translations()),
        size = "xl",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }) |>
      bindEvent(input$previewReport)

    output$reportDlPdf <- downloadHandler(
      filename = function() {
        trans <- translations()
        paste0(tr("ui_bcanz_filename", trans), ".pdf")
      },
      content = function(file) {
        report <- req(current_report())
        params <- report$params
        params$pred_cl <- report$pred_cl
        render_report(
          report$template,
          params,
          output_format = "pdf_document",
          output_file = file
        )
      }
    )

    output$reportDlHtml <- downloadHandler(
      filename = function() {
        trans <- translations()
        paste0(tr("ui_bcanz_filename", trans), ".html")
      },
      content = function(file) {
        writeLines(req(current_report())$html, file)
      }
    )

    # downloads ---------------------------------------------------------------
    output$has_data <- data_mod$has_data
    output$has_fit <- fit_mod$has_fit
    output$has_cl <- predict_mod$has_cl
    for (name in c("has_data", "has_fit", "has_cl")) {
      outputOptions(output, name, suspendWhenHidden = FALSE)
    }

    save_png <- function(plot, file) {
      ggplot2::ggsave(file, plot = plot, device = "png", width = input$width, height = input$height, dpi = input$dpi)
    }
    save_table <- function(table, file, format) {
      table <- dplyr::as_tibble(table)
      if (format == "csv") readr::write_csv(table, file) else writexl::write_xlsx(table, file)
    }
    tables <- list(
      data = list(name = "ssdtools_data", value = function() data_mod$current_data()),
      gof = list(name = "ssdtools_gof_table", value = function() fit_mod$gof_table()),
      cl = list(name = "ssdtools_cl_table", value = function() predict_mod$predict_cl())
    )
    plots <- list(
      fit = list(name = "ssdtools_distFitPlot", value = function() fit_mod$fit_plot()),
      pred = list(name = "ssdtools_model_average_plot", value = function() predict_mod$model_average_plot())
    )
    table_handler <- function(table, format) {
      downloadHandler(
        filename = function() paste0(table$name, ".", format),
        content = function(file) save_table(table$value(), file, format)
      )
    }
    output$dataDlCsv <- table_handler(tables$data, "csv")
    output$dataDlXlsx <- table_handler(tables$data, "xlsx")
    output$fitDlCsv <- table_handler(tables$gof, "csv")
    output$fitDlXlsx <- table_handler(tables$gof, "xlsx")
    output$predDlCsv <- table_handler(tables$cl, "csv")
    output$predDlXlsx <- table_handler(tables$cl, "xlsx")
    plot_handler <- function(plot, format) {
      downloadHandler(
        filename = function() paste0(plot$name, ".", format),
        content = function(file) {
          if (format == "png") save_png(plot$value(), file) else saveRDS(plot$value(), file)
        }
      )
    }
    output$fitDlPlot <- plot_handler(plots$fit, "png")
    output$fitDlRds <- plot_handler(plots$fit, "rds")
    output$predDlPlot <- plot_handler(plots$pred, "png")
    output$predDlRds <- plot_handler(plots$pred, "rds")

    # The report's own files: the report as HTML and PDF and its hazard
    # concentrations. The PDF is left out when it cannot be rendered (it needs
    # LaTeX).
    write_report_files <- function(report, dir) {
      name <- tr("ui_bcanz_filename", translations())
      writeLines(report$html, file.path(dir, paste0(name, ".html")))
      params <- report$params
      params$pred_cl <- report$pred_cl
      try(render_report(report$template, params, "pdf_document", file.path(dir, paste0(name, ".pdf"))), silent = TRUE)
      save_table(report$pred_cl, file.path(dir, paste0(name, "_hc.csv")), "csv")
      save_table(report$pred_cl, file.path(dir, paste0(name, "_hc.xlsx")), "xlsx")
    }

    zip_dir <- function(dir, file) {
      utils::zip(file, list.files(dir, full.names = TRUE), flags = "-jq")
    }

    # Every file there is to download, in one ZIP: each table and plot that
    # exists in every format, the report's files when it is current, and the
    # R script.
    output$downloadAll <- downloadHandler(
      filename = function() "ssdtools.zip",
      content = function(file) {
        dir <- tempfile("ssdtools-")
        dir.create(dir)
        on.exit(unlink(dir, recursive = TRUE), add = TRUE)
        try_save <- function(name, save) {
          path <- file.path(dir, name)
          result <- try(save(path), silent = TRUE)
          if (inherits(result, "try-error")) unlink(path)
        }
        for (table in tables) {
          for (format in c("csv", "xlsx")) {
            try_save(paste0(table$name, ".", format), function(path) save_table(table$value(), path, format))
          }
        }
        for (plot in plots) {
          try_save(paste0(plot$name, ".png"), function(path) save_png(plot$value(), path))
          try_save(paste0(plot$name, ".rds"), function(path) saveRDS(plot$value(), path))
        }
        report <- current_report()
        if (!is.null(report)) write_report_files(report, dir)
        script <- code()
        if (length(script) && nzchar(script)) writeLines(script, file.path(dir, "ssdtools-analysis.R"))
        zip_dir(dir, file)
      }
    )

    # Every BCANZ output in one ZIP: the report's files, and the data, plots
    # and tables it shows.
    output$bcanzZip <- downloadHandler(
      filename = function() paste0(tr("ui_bcanz_filename", translations()), ".zip"),
      content = function(file) {
        report <- req(current_report())
        dir <- tempfile("bcanz-")
        dir.create(dir)
        on.exit(unlink(dir, recursive = TRUE), add = TRUE)
        write_report_files(report, dir)
        save_table(report$params$data, file.path(dir, "ssdtools_data.csv"), "csv")
        save_table(report$params$gof_table, file.path(dir, "ssdtools_gof_table.csv"), "csv")
        save_png(report$params$fit_plot, file.path(dir, "ssdtools_distFitPlot.png"))
        save_png(report$params$model_average_plot, file.path(dir, "ssdtools_model_average_plot.png"))
        zip_dir(dir, file)
      }
    )

    observe_step_button(input, "goPredict", "predict")

    list(
      running = report_runner$running,
      has_preview = has_preview,
      width = reactive(input$width),
      height = reactive(input$height),
      dpi = reactive(input$dpi)
    )
  })
}
