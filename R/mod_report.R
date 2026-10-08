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
          width = 350,
          style = "height: calc(100vh - 150px); overflow-y: auto; overflow-x: hidden;",
          tagList(
            div(
              h5(span(`data-translate` = "ui_tabreport", "Get BCANZ report")),
            ) |>
              shinyhelper::helper(
                type = "markdown",
                content = "reportTab",
                size = "l",
                colour = color_primary,
                buttonLabel = "OK"
              ),
            textInput(
              ns("toxicant"),
              label = span(`data-translate` = "ui_4toxname", "Toxicant name"),
              value = ""
            ),
            selectizeInput(
              ns("bootSamp"),
              options = list(
                create = TRUE,
                createFilter = "^(?:[1-9][0-9]{0,3}|10000)$"
              ),
              label = span(
                `data-translate` = "ui_3samples",
                "Bootstrap samples"
              ),
              choices = c("500", "1,000", "5,000", "10,000"),
              selected = "10,000"
            ),
            # Get Report shows until the model-averaged curve is bootstrapped;
            # from then on the report renders by itself.
            conditionalPanel(
              condition = sprintf(
                "!%s && %s",
                paste_js("report_running", ns),
                paste_js("needs_bootstrap", ns)
              ),
              button(
                ns("generateReport"),
                span(`data-translate` = "ui_getreport", "Get Report"),
                icon = bsicons::bs_icon("file-earmark-text"),
                class = "w-100"
              ),
              shiny::helpText(htmlOutput(ns("describeTime")))
            ),
            conditionalPanel(
              condition = paste_js("report_running", ns),
              notice(
                icon = busy_icon(),
                title = span(`data-translate` = "ui_4gentitle", "Generating report..."),
                tone = "info",
                action = button(
                  ns("cancelReport"),
                  span(`data-translate` = "ui_cancel", "Cancel"),
                  icon = bsicons::bs_icon("x-lg"),
                  variant = "outline",
                  size = "sm"
                )
              )
            )
          )
        ),
        div(
          class = "p-3",
          conditionalPanel(
            condition = paste_js("has_preview", ns),
            card(
              class = card_shadow,
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span(`data-translate` = "ui_prevreport", "Preview report")
              ),
              card_body(
                padding = 25,
                ui_download_report(ns = ns),
                tags$iframe(
                  srcdoc = "",
                  id = ns("htmlPreview"),
                  style = "width: 100%; height: 600px; border: 1px solid #ddd; border-radius: 4px; background: white;",
                  sandbox = "allow-same-origin allow-scripts allow-popups allow-popups-to-escape-sandbox"
                )
              )
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = paste0("!output['", ns("has_predict"), "']"),
empty_state(
        icon = bsicons::bs_icon("calculator"),
        title = span(
          `data-translate` = "ui_hintpredict",
          "You have not successfully generated predictions yet. Run the 'Predict' tab first."
        ),
        action = step_button(ns("goPredict"), "ui_goto_predict", "Go to Predict", variant = "outline")
      )
    )
  )
}

# Report Module Server
mod_report_server <- function(
  id,
  translations,
  lang,
  data_mod,
  fit_mod,
  predict_mod,
  shared_toxicant_name = NULL,
  main_nav = reactive("report")
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$has_predict <- predict_mod$has_predict
    outputOptions(output, "has_predict", suspendWhenHidden = FALSE)

    observe({
      current <- lang()
      nboot_value <- predict_mod$nboot()

      # Default choices based on language
      if (current == "french") {
        choices <- c("500", "1 000", "5 000", "10 000")
        standard_values <- c("500", "1000", "5000", "10000")
      } else {
        choices <- c("500", "1,000", "5,000", "10,000")
        standard_values <- c("500", "1000", "5000", "10000")
      }

      # Check if nboot_value is a custom value (not in standard list)
      nboot_clean <- clean_nboot(nboot_value)
      if (!is.null(nboot_value) && !nboot_clean %in% standard_values) {
        choices <- c(choices, nboot_value)
      }

      updateSelectizeInput(
        session,
        "bootSamp",
        choices = choices,
        selected = nboot_value
      )
    }) |>
      bindEvent(lang(), predict_mod$nboot())

    # Update toxicant input when shared value changes from another module
    if (!is.null(shared_toxicant_name)) {
      observe({
        toxicant_name <- shared_toxicant_name()
        if (!is.null(toxicant_name) && toxicant_name != "" &&
            toxicant_name != input$toxicant) {
          updateTextInput(
            session,
            "toxicant",
            value = toxicant_name
          )
        }
      }) |>
        bindEvent(shared_toxicant_name())

      # Update shared value when this module's input changes
      observe({
        shared_toxicant_name(input$toxicant)
      }) |>
        bindEvent(input$toxicant)
    }

    # The report's parameters other than its confidence limits, which the
    # report job adds.
    params_list <- reactive({
      req(predict_mod$has_predict())
      req(fit_mod$has_fit())

      list(
        toxicant = input$toxicant,
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

    report_nboot <- reactive(clean_nboot(req(input$bootSamp)))

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
      req(main_nav() == "report")
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

    output$describeTime <- renderText({
      HTML(
        tr("ui_3cldesc3", translations()),
        estimate_time(report_nboot(), lang()),
        tr("ui_3cldesc4", translations())
      )
    })

    # Links open in a new tab rather than within the preview's iframe.
    report_preview_html <- reactive({
      html <- req(current_report())$html
      gsub("<a href=", "<a target=\"_blank\" href=", html, fixed = TRUE)
    })

    has_preview <- reactive(!is.null(current_report()))

    output$has_preview <- has_preview
    outputOptions(output, "has_preview", suspendWhenHidden = FALSE)

    # Update iframe content with HTML
    observe({
      shinyjs::runjs(paste0(
        "var iframe = document.getElementById('",
        ns("htmlPreview"),
        "'); if (iframe) { iframe.srcdoc = ",
        jsonlite::toJSON(report_preview_html()),
        "; }"
      ))
    }) |>
      bindEvent(report_preview_html())

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

    observe_step_button(input, "goPredict", "predict")

    list(running = report_runner$running, has_preview = has_preview)
  })
}
