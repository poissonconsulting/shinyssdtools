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

# Fit Module UI
mod_fit_ui <- function(id) {
  ns <- NS(id)

  title <- span(`data-translate` = "ui_tabfit", "Fit distributions") |>
    shinyhelper::helper(type = "markdown", content = "fitTab", size = "l", colour = color_primary, buttonLabel = "OK")

  aside <- card(card_body(
    tags$label(
      `for` = ns("selectConc"),
      class = "control-label",
      span(`data-translate` = "ui_2conc", "Concentration")
    ),
    selectInput(ns("selectConc"), label = NULL, choices = NULL, selected = NULL, width = "100%"),
    selectizeInput(
      ns("selectDist"),
      label = span(`data-translate` = "ui_2dist", "Select distributions to fit"),
      multiple = TRUE,
      choices = c(default.dists, extra.dists),
      selected = default.dists,
      options = list("plugins" = list("remove_button")),
      width = "100%"
    ),
    checkboxInput(
      ns("rescale"),
      label = span(`data-translate` = "ui_2rescale", "Rescale"),
      value = FALSE
    ),
    actionButton(
      ns("updateFit"),
      # Both icons are in the page and switched in the browser, so the
      # button does not re-render (and resize) as the fit goes out of date.
      label = span(
        class = "d-inline-flex align-items-center gap-2",
        span(`data-display-if` = paste_js("fit_stale", ns), `data-ns-prefix` = "", lucide("refresh-cw")),
        span(`data-display-if` = sprintf("!%s", paste_js("fit_stale", ns)), `data-ns-prefix` = "", lucide("check-circle-2")),
        span(`data-translate` = "ui_update_fit", "Update fit")
      ),
      # Outline while the fit is current, tinted while it is out of date.
      class = "btn-light border w-100"
    ),
    accordion(
      class = "mt-2",
      open = FALSE,
      accordion_panel(
        title = span(`data-translate` = "ui_3plotopts", "Plot formatting options"),
        value = "plot_format_fit",
        icon = lucide("sliders-horizontal"),
        selectInput(
          ns("selectUnit"),
          label = span(`data-translate` = "ui_2unit", "Select units"),
          choices = units(),
          selected = units()[1]
        ),
        textInput(
          ns("xaxis2"),
          label = span(`data-translate` = "ui_3xlab", "X-axis label"),
          value = "Concentration"
        ),
        textInput(
          ns("yaxis2"),
          label = span(`data-translate` = "ui_3ylab", "Y-axis label"),
          value = "Species affected (%)"
        ),
        numericInput(
          ns("size2"),
          label = span(`data-translate` = "ui_size", "Text size"),
          value = 12,
          min = 1,
          max = 100
        ),
        textInput(
          ns("title"),
          value = "",
          label = span(`data-translate` = "ui_3title", "Title")
        )
      )
    )
  ))

  main <- tagList(
    page_header(
      title,
      conditionalPanel(
        condition = paste_js("has_fit", ns),
        step_button(ns("continue"), "ui_continue_predict", "Continue to predict")
      )
    ),
    # Outside the panels' column, so they take no space while there is no
    # error and the fit is current.
    uiOutput(ns("fitError"), class = "ssd-fit-error"),
    # While the first fit runs, a placeholder where its plot will be; shown by
    # style.css while fitError, which depends on the fit, is recalculating.
    conditionalPanel(
      condition = sprintf("!%s", paste_js("has_fit", ns)),
      div(
        class = "ssd-fit-loading",
        panel(
          span(`data-translate` = "ui_2plot", "Fitted distributions"),
          div(
            class = "ssd-figure ssd-fit-loading-figure d-flex align-items-center justify-content-center gap-2 text-body-secondary",
            role = "status",
            busy_icon(),
            span(`data-translate` = "ui_fitting", "Fitting distributions...")
          )
        )
      )
    ),
    # Without a fit, why the data cannot be fitted, in place of the results.
    conditionalPanel(
      condition = sprintf("!%s && %s", paste_js("has_fit", ns), paste_js("has_conc_problem", ns)),
      empty_state(
        "alert-triangle",
        textOutput(ns("concProblem"), inline = TRUE),
        action = step_button(ns("goDataProblem"), "ui_goto_data", "Go to Data", variant = "outline")
      )
    ),
    conditionalPanel(
      condition = sprintf("%s && %s", paste_js("has_fit", ns), paste_js("fit_stale", ns)),
      div(
        class = "mb-4",
        notice(
          "alert-triangle",
          span(`data-translate` = "ui_fit_stale", "The fit is out of date"),
          tone = "warning",
          action = button(
            ns("updateFitNotice"),
            span(`data-translate` = "ui_update_fit", "Update fit"),
            icon = "refresh-cw",
            variant = "outline",
            size = "sm"
          )
        )
      )
    ),
    div(
      class = "d-flex flex-column gap-4",
      conditionalPanel(
        condition = paste_js("has_fit", ns),
        div(
          class = "d-flex flex-column gap-4",
          panel(
            span(`data-translate` = "ui_2plot", "Fitted distributions"),
            htmlOutput(ns("fitFail")),
            div(class = "ssd-figure", plotOutput(ns("plotDist")))
          ),
          panel(
            span(`data-translate` = "ui_2table", "Goodness of fit table") |>
              shinyhelper::helper(type = "markdown", content = "gofTable", size = "l", colour = color_primary, buttonLabel = "OK"),
            reactable::reactableOutput(ns("tableGof"))
          )
        )
      )
    )
  )

  tagList(
    conditionalPanel(
      condition = paste_js("has_data", ns),
      step_layout(aside, main)
    ),
    conditionalPanel(
      condition = sprintf("!%s", paste_js("has_data", ns)),
      page_header(title),
      empty_state(
        "table",
        span(`data-translate` = "ui_hintdata", "You have not added a dataset."),
        action = step_button(ns("goData"), "ui_goto_data", "Go to Data", variant = "outline")
      )
    )
  )
}

mod_fit_server <- function(
  id,
  translations,
  lang,
  data_mod,
  big_mark,
  decimal_mark,
  main_nav
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$has_data <- data_mod$has_data
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    fit_trigger <- reactiveVal(0)

    # The distributions and rescaling of the last fit: the fit is out of date
    # while the current choices differ from them.
    fit_settings <- reactive(list(dists = sort(input$selectDist), rescale = isTRUE(input$rescale)))
    fitted_settings <- reactiveVal(NULL)
    # The data of the last fit: the fit, and the predictions, confidence
    # limits and report computed from it, no longer apply once the data
    # change.
    fitted_data <- reactiveVal(NULL)
    # The inputs of the last fit: refitting the same inputs would return the
    # cached fit but still invalidate, and so re-render, everything downstream.
    fitted_inputs <- NULL
    refit <- function() {
      # The validation rules need a concentration column to check.
      valid <- !is.null(isolate(input$selectConc)) &&
        isTRUE(tryCatch(isolate(iv$is_valid()), error = function(e) FALSE))
      if (!valid) {
        return()
      }
      inputs <- list(isolate(fit_settings()), isolate(input$selectConc), isolate(data_mod$data()))
      if (identical(inputs, fitted_inputs)) {
        return()
      }
      fitted_inputs <<- inputs
      fitted_settings(isolate(fit_settings()))
      fitted_data(isolate(data_mod$data()))
      fit_trigger(isolate(fit_trigger()) + 1)
    }
    fit_stale <- reactive(!is.null(fitted_settings()) && !identical(fit_settings(), fitted_settings()))
    output$fit_stale <- fit_stale
    outputOptions(output, "fit_stale", suspendWhenHidden = FALSE)
    observe({
      stale <- fit_stale()
      shinyjs::toggleClass("updateFit", "ssd-btn-soft", condition = stale)
      shinyjs::toggleClass("updateFit", "border", condition = !stale)
    })

    # trigger if navigate to fit tab
    observe({
      if (main_nav() == "fit") refit()
    }) |>
      bindEvent(main_nav())

    # Also when Update Fit is clicked, in the aside or in the out of date
    # notice: one observer for each, as bound to both, bindEvent() runs as the
    # buttons start up.
    observe(refit()) |>
      bindEvent(input$updateFit)
    observe(refit()) |>
      bindEvent(input$updateFitNotice)

    # Auto-update for critical changes
    observe({
      if (isolate(main_nav()) == "fit") refit()
    }) |>
      bindEvent(input$selectConc, data_mod$data(), ignoreInit = TRUE)

    # The fit, or the error when no distribution could be fitted, so the error
    # can be shown rather than an empty tab.
    fit_result <- reactive({
      req(fit_trigger() > 0)
      req(main_nav() == "fit")
      req(data_mod$data())
      req(input$selectConc)
      req(input$selectDist)
      req(iv$is_valid())

      data <- data_mod$data()
      conc <- make.names(input$selectConc)
      dists <- input$selectDist
      rescale <- input$rescale

      tryCatch(ssdtools::ssd_fit_bcanz(
        data,
        left = conc,
        dists = dists,
        silent = TRUE,
        rescale = rescale
      ), error = function(e) e)
    }) |>
      # Sorted, so the same distributions in another order are the same fit,
      # and the confidence limits and reports computed for it still apply.
      bindCache(
        input$selectConc,
        sort(input$selectDist),
        input$rescale,
        data_mod$data()
      ) |>
      bindEvent(fit_trigger())

    # The fit or its error while it is of the current data, else NULL.
    current_result <- reactive({
      result <- fit_result()
      if (identical(fitted_data(), data_mod$data())) result
    })

    fit_dist <- reactive({
      result <- current_result()
      if (!inherits(result, "error")) result
    })

    output$fitError <- renderUI({
      result <- current_result()
      req(inherits(result, "error"))
      div(
        class = "mb-4",
        notice(
          icon = "x-circle",
          title = tr("ui_fit_failed", translations()),
          conditionMessage(result),
          tone = "danger"
        )
      )
    })


    observe({
      data <- data_mod$clean_data()
      choices <- names(data)
      selected <- guess_conc(choices, data)
      if (is.na(selected)) {
        selected <- choices[1]
      }
      updateSelectInput(
        session,
        "selectConc",
        choices = choices,
        selected = selected
      )
    }) |>
      bindEvent(data_mod$clean_data())

    observe({
      toxicant_name <- data_mod$toxicant_name()
      if (!is.null(toxicant_name) && toxicant_name != "") {
        updateTextInput(
          session,
          "title",
          value = toxicant_name
        )
      }
    }) |>
      bindEvent(data_mod$toxicant_name())

    observe({
      trans <- translations()
      updateTextInput(session, "yaxis2", value = tr("ui_2ploty", trans))
    }) |>
      bindEvent(translations())

    # validation --------------------------------------------------------------
    iv <- InputValidator$new()

    # Why the chosen concentration column cannot be fitted, or NULL.
    conc_problem <- function(value) {
      trans <- translations()
      dat <- data_mod$data()

      conc_data <- dat[[make.names(value)]]

      if (!has_numeric_concentration(conc_data)) {
        return(as.character(tr("ui_hintnum", trans)[1]))
      }
      if (!has_no_missing_concentration(conc_data)) {
        return(as.character(tr("ui_hintmiss", trans)[1]))
      }
      if (!has_positive_concentration(conc_data)) {
        return(as.character(tr("ui_hintpos", trans)[1]))
      }
      if (!has_finite_concentration(conc_data)) {
        return(as.character(tr("ui_hintfin", trans)[1]))
      }
      if (!has_min_concentration(conc_data)) {
        return(as.character(tr("ui_hint6", trans)[1]))
      }
      if (!has_not_all_identical(conc_data)) {
        return(as.character(tr("ui_hintident", trans)[1]))
      }

      NULL
    }

    iv$add_rule("selectConc", conc_problem)

    current_conc_problem <- reactive({
      req(data_mod$has_data(), input$selectConc)
      conc_problem(input$selectConc)
    })
    output$has_conc_problem <- reactive(!is.null(current_conc_problem()))
    outputOptions(output, "has_conc_problem", suspendWhenHidden = FALSE)
    output$concProblem <- renderText(current_conc_problem())

    iv$add_rule("selectDist", function(value) {
      trans <- translations()
      if (is.null(value) || length(value) == 0) {
        return(as.character(tr("ui_hintdist", trans)[1]))
      }
      NULL
    })

    iv$enable()

    # fit reactives and outputs -----------------------------------------------
    plot_dist <- reactive({
      dist <- fit_dist()
      req(dist)

      plot_distributions(
        dist,
        ylab = input$yaxis2,
        xlab = append_unit(input$xaxis2, input$selectUnit),
        text_size = input$size2,
        big.mark = big_mark(),
        decimal.mark = decimal_mark(),
        title = input$title
      )
    })

    table_gof <- reactive({
      dist <- fit_dist()
      req(dist)

      trans <- translations()
      gof <-
        ssdtools::ssd_gof(dist, wt = TRUE) |>
        # Remove at_bound and computable columns
        dplyr::select(-at_bound, -computable) |>
        # Round different columns to different sig figs
        dplyr::mutate(
          dplyr::across(c(log_lik, aic, aicc, bic), ~ signif(.x, 4))
        ) |>
        dplyr::mutate_if(is.numeric, ~ signif(., 3)) |>
        dplyr::arrange(dplyr::desc(.data$wt))
      names(gof) <- gsub("weight", tr("ui_2weight", trans), names(gof))
      gof
    })

    output$plotDist <- renderPlot(
      {
        plot_dist()
      },
      alt = reactive({
        switch(
          lang(),
          "french" = "Graphique de distribution de sensibilit\u00e9 des esp\u00e8ces montrant les courbes de distribution ajust\u00e9es superpos\u00e9es aux donn\u00e9es de concentration observ\u00e9es pour chaque esp\u00e8ce. L'axe des x indique les valeurs de concentration et l'axe des y indique la proportion des esp\u00e8ces affect\u00e9es.",
          "spanish" = "Gr\u00e1fico de distribuci\u00f3n de sensibilidad de especies que muestra curvas de distribuci\u00f3n ajustadas superpuestas a los datos de concentraci\u00f3n de especies observados. El eje x muestra los valores de concentraci\u00f3n y el eje y muestra la proporci\u00f3n de especies afectadas.",
          "Species Sensitivity Distribution plot showing fitted distribution curves overlaid on observed species concentration data. The x-axis shows concentration values and the y-axis shows the proportion of species affected."
        )
      })
    )

    output$tableGof <- reactable::renderReactable({
      gof <- table_gof()
      trans <- translations()
      weight <- intersect(c("wt", "weight", tr("ui_2weight", trans)), names(gof))[1]
      # The weight beside the distribution, where it is seen first; the
      # downloads keep the ssdtools column order.
      gof <- dplyr::relocate(gof, dplyr::all_of(weight), .after = 1)
      app_table(
        as.data.frame(gof),
        lang = lang(),
        tooltips = gof_header_tooltips(trans, lang()),
        weight = weight,
        pagination = FALSE
      )
    })

    # Notify when failed fits
    fit_fail <- reactive({
      dist <- fit_dist()
      paste0(setdiff(input$selectDist, names(dist)), collapse = ", ")
    }) |>
      bindEvent(fit_dist())

    output$fitFail <- renderText({
      failed <- fit_fail()
      req(failed != "")
      span(
        class = "text-body-secondary",
        paste(failed, tr("ui_hintfail", translations()))
      )
    }) |>
      bindEvent(fit_fail())

    observe_step_button(input, "continue", "predict")
    observe_step_button(input, "goData", "data")
    observe_step_button(input, "goDataProblem", "data")

    # return values ------------------------------------------------------------
    # The fit stays while an input is being edited (such as every distribution
    # removed before choosing others); the validation message shows on the
    # input, and Update Fit does not refit until the inputs are valid.
    has_fit <- reactive(!is.null(fit_dist()))

    output$has_fit <- has_fit
    outputOptions(output, "has_fit", suspendWhenHidden = FALSE)

    return(
      list(
        fit_dist = fit_dist,
        fit_plot = plot_dist,
        gof_table = table_gof,
        big_mark = big_mark,
        decimal_mark = decimal_mark,
        conc_column = reactive({
          input$selectConc
        }),
        units = reactive({
          input$selectUnit
        }),
        dists = reactive({
          input$selectDist
        }),
        rescale = reactive({
          input$rescale
        }),
        yaxis_label = reactive({
          input$yaxis2
        }),
        xaxis_label = reactive({
          input$xaxis2
        }),
        text_size = reactive({
          input$size2
        }),
        title = reactive({
          input$title
        }),
        has_fit = has_fit
      )
    )
  })
}
