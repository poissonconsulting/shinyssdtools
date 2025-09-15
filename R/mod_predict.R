# Predict Module UI
mod_predict_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    conditionalPanel(
      condition = paste_js("has_fit", ns),
      layout_sidebar(
        padding = "1rem",
        gap = "1rem",
        sidebar = sidebar(
          width = 350,
          bslib::accordion(
            open = c("hc_pred", "cl_pred"),
            accordion_panel(
              title = span(`data-translate` = "ui_3est", "Estimate hazard concentration"),
              value = "hc_pred",
              radioButtons(ns("thresh_type"), 
                           label = span(`data-translate` = "ui_3threshlabel", "Threshold type"),
                           choices = c("Concentration" = "Concentration", "Fraction affected" = "Fraction"),
                           selected = "Concentration", inline = TRUE
              ),
              uiOutput(ns("ui_3thresh"))
            ),
          # section_break("Estimate hazard concentration"),
          accordion_panel(
            title = span(`data-translate` = "ui_3cl", "Get confidence limits"),
            value = "cl_pred",
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
              choices = c(
                "500" = "500",
                "1,000" = "1000",
                "5,000" = "5000",
                "10,000" = "10000"
              ),
              selected = "1000",
              width = "190px"
            ),
            actionButton(
              ns("getCl"),
              label = tagList(
                bsicons::bs_icon("calculator"), 
                span(`data-translate` = "ui_3clbutton", "Get CL")
              ),
              class = "btn-primary w-100",
            ),
            shiny::helpText( htmlOutput(ns("describeTime")))
          ),
          # section_break("Get confidence limits"),
            bslib::accordion_panel(
              title = span(`data-translate` = "ui_3plotopts", "Plot formatting options"),
              value = "plot_format_pred",
              selected = FALSE, 
              uiOutput(ns("selectLabel")),
              uiOutput(ns("selectColour")),
              uiOutput(ns("selectShape")),
              selectInput(ns("selectPalette"), 
                          label = span(`data-translate` = "ui_3pal", "Palette"), 
                          choices = pals, selected = pals[2]),
              textInput(ns("xaxis"), 
                        value = "Concentration", 
                        label = span(`data-translate` = "ui_3xlab", "X-axis label")),
              textInput(ns("yaxis"), 
                        value = "Percent", 
                        label = span(`data-translate` = "ui_3ylab", "Y-axis label")),
              textInput(ns("title"), 
                        value = "", 
                        label = span(`data-translate` = "ui_3title", "Title")),
              uiOutput(ns("uiLegendColour")),
              uiOutput(ns("uiLegendShape")),
              layout_column_wrap(width = 1 / 2, 
                                 numericInput(ns("size3"), 
                                              label = span(`data-translate` = "ui_size", "Text size"), 
                                              value = 12, min = 1, max = 100),
                                 numericInput(ns("sizeLabel3"), 
                                              label = span(`data-translate` = "ui_sizeLabel", "Label size"), 
                                              value = 3, min = 1, max = 10)
              ),
              checkboxInput(ns("checkHc"), 
                            label = span(`data-translate` = "ui_checkHc", "Show hazard concentration"), 
                            value = TRUE),
              layout_column_wrap(
                width = 1 / 3,
                numericInput(ns("adjustLabel"),
                             value = 1.05, 
                             label = span(`data-translate` = "ui_adjustLabel", "Adjust label"), 
                             min = 0, max = 10, step = 0.1),
                numericInput(ns("xMin"), 
                             label = span(`data-translate` = "ui_xmin", "X min"), 
                             min = 1, value = NULL),
                numericInput(ns("xMax"), 
                             label = span(`data-translate` = "ui_xmax", "X max"), 
                             min = 1, value = NULL)
              ),
              checkboxInput(ns("xlog"), 
                            label = span(`data-translate` = "ui_xlog", "Log scale"), 
                            value = TRUE),
              uiOutput(ns("uiXbreaks"))
            )
            ),
        ),
        div(
          class = "p-3",
          conditionalPanel(
            condition = paste_js('has_predict', ns),
            div(id = ns("divPred"),
                card(
                  full_screen = TRUE,
                  card_header(
                    class = "d-flex justify-content-between align-items-center",
                    span(`data-translate` = "ui_3model", "Model Average Plot")
                  ),
                  card_body(
                    ui_download_popover(tab = "pred", ns = ns),
                    plotOutput(ns("plotPred")),
                    div(
                      conditionalPanel(
                        condition = glue::glue("input['{ns(\"thresh_type\")}'] == 'Concentration'"),
                        div(
                          span("HC"), 
                          textOutput(ns("hc_percent"), inline = TRUE),
                          span("/ PC"), 
                          textOutput(ns("pc_percent"), inline = TRUE),
                          span(": "), 
                          tags$b(textOutput(ns("hc_conc"), inline = TRUE))
                        ),
                        div(
                          span(`data-translate` = "ui_3hc", "The model averaged estimate of the concentration that affects "),
                          tags$b(textOutput(ns("estPerc"), inline = TRUE)),
                          span(`data-translate` = "ui_3hc2", " % of species is "),
                          tags$b(textOutput(ns("estConc"), inline = TRUE))
                        )
                      ),
                      conditionalPanel(
                        condition = glue::glue("input['{ns(\"thresh_type\")}'] != 'Concentration'"),
                        div(
                          span(`data-translate` = "ui_3perc", "The model averaged estimate of the fraction affected by a concentration of "),
                          tags$b(textOutput(ns("estConc2"), inline = TRUE)),
                          span(`data-translate` = "ui_3perc2", " is "),
                          tags$b(textOutput(ns("estPerc2"), inline = TRUE)),
                          span(`data-translate` = "ui_3perc3", " % of species")
                        )
                      )
                    )
                  )
                )
            ),
            conditionalPanel(
              condition = paste_js("has_cl", ns = ns),
              card(
                full_screen = TRUE,
                card_header(
                  class = "d-flex justify-content-between align-items-center",
                  div(
                    span(`data-translate` = "ui_3cl", "Confidence Limits"),
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle", style = "margin-left: 0.5rem; color: #6c757d; outline: none; border: none;"),
                      uiOutput(ns("ui_3help")),
                      placement = "right"
                    )
                  )
                ),
                card_body(padding = 25,
                  div(class = "table-responsive",
                      
                      DT::dataTableOutput(ns("clTable")))
                )
              )
            )
        )))
    ),
    conditionalPanel(
      condition =  paste0("!output['", ns("has_fit"), "']"),
      ui_dashbox(span(`data-translate` = "ui_hintfit", "You have not successfully fit any distributions yet. Run the 'Fit' tab first."))
    )
  )
  

}

# Predict Module Server
mod_predict_server <- function(id, translations, lang, data_mod, fit_mod, main_nav = reactive("predict")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$has_fit <- fit_mod$has_fit
    outputOptions(output, "has_fit", suspendWhenHidden = FALSE)
    
    # Predict trigger - similar to fit_trigger pattern
    predict_trigger <- reactiveVal(0)
    
    # Trigger when navigate to predict tab
    observe({
      if(main_nav() == "predict") {
        current_val <- isolate(predict_trigger())
        predict_trigger(current_val + 1)
      }
    }) %>%
      bindEvent(main_nav())
    
    output$estConc <- renderText({
      thresh_rv$conc
    })
    
    output$estPerc <- renderText({
      thresh_rv$percent
    })
    
    output$estConc2 <- renderText({
      thresh_rv$conc
    })
    
    output$estPerc2 <- renderText({
      thresh_rv$percent
    })
    
    # Also trigger when threshold values change
    observe({
      if(isolate(main_nav()) == "predict") {
        current_val <- isolate(predict_trigger())
        predict_trigger(current_val + 1)
      }
    }) %>%
      bindEvent(thresh_rv$percent, thresh_rv$conc, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observe({
      trans <- translations()
      choices <- c(
        "Concentration",
        tr("ui_3thresh", trans)
      )
      updateRadioButtons(session, "thresh_type", 
                        choices = choices,
                        selected = input$thresh_type %||% "Concentration")
    }) %>%
      bindEvent(translations())
    
    thresh_rv <- reactiveValues(
      percent = NULL,
      conc = NULL
    )
    
    column_names <- reactive({
      names(data_mod$clean_data())
    })
     
# output renders ----------------------------------------------------------
    output$selectLabel <- renderUI({
      cols <- column_names()
      selectInput(ns("selectLabel"),
        label = span(`data-translate` = "ui_3label", "Label"),
        choices = c("-none-", cols),
        selected = guess_sp(cols)
      )
    })
    
    output$selectColour <- renderUI({
      selectInput(ns("selectColour"),
        label = span(`data-translate` = "ui_3colour", "Colour"),
        choices = c("-none-", column_names()),
        selected = "-none-"
      )
    })
    
    output$selectShape <- renderUI({
      selectInput(ns("selectShape"),
        label = span(`data-translate` = "ui_3symbol", "Symbol"),
        choices = c("-none-", column_names()),
        selected = "-none-"
      )
    })
    
    output$uiLegendColour <- renderUI({
      textInput(ns("legendColour"), 
                label = span(`data-translate` = "ui_3legend", "Legend colour"), 
                value = input$selectColour)
    })
    
    output$uiLegendShape <- renderUI({
      textInput(ns("legendShape"), 
                label = span(`data-translate` = "ui_3shape", "Legend shape"), 
                value = input$selectShape)
    })
    
    # X-breaks UI
    output$uiXbreaks <- renderUI({
      xbreaks <- plot_model_average_xbreaks()
      selectizeInput(ns("xbreaks"),
                     label = span(`data-translate` = "ui_xbreaks", "X breaks"),
                     options = list(create = TRUE, plugins = list("remove_button")),
                     choices = xbreaks,
                     selected = xbreaks,
                     multiple = TRUE
      )
    })
    
    outputOptions(output, "uiXbreaks", suspendWhenHidden = FALSE)
    outputOptions(output, "uiLegendColour", suspendWhenHidden = FALSE)
    outputOptions(output, "uiLegendShape", suspendWhenHidden = FALSE)
    outputOptions(output, "selectLabel", suspendWhenHidden = FALSE)
    outputOptions(output, "selectColour", suspendWhenHidden = FALSE)
    outputOptions(output, "selectShape", suspendWhenHidden = FALSE)
    
    output$ui_3thresh <- renderUI({
      req(input$thresh_type)
      if (input$thresh_type != "Concentration") {
        return(
          layout_column_wrap(
            width = 1/2,
            numericInput(ns("conc"),
                         label = span(`data-translate` = "ui_3byconc", "by concentration"),
                         value = 1, min = 0,
                         max = 100, step = 0.1
            )
          ))
      }
      layout_column_wrap(
        width = 1/2,
        selectizeInput(ns("thresh"),
                       label = span(`data-translate` = "ui_3affecting", "% affecting"),
                       choices = c(1, 5, 10, 20),
                       options = list(create = TRUE, createFilter = "^[1-9][0-9]?$|^99$"),
                       selected = 5
        ),
        selectizeInput(ns("thresh_pc"),
                       label = span(`data-translate` = "ui_3protecting", "% protecting"),
                       choices = c(99, 95, 90, 80),
                       options = list(create = TRUE, createFilter = "^[1-9][0-9]?$|^99$"),
                       selected = 95
        )
      )
    })
  
    # Threshold logic observers
    observe({
      thresh_pc <- 100 - as.numeric(input$thresh)
      choices <- unique(c(99, 95, 90, 80, thresh_pc))
      updateSelectizeInput(session, "thresh_pc", choices = choices, selected = isolate(thresh_pc))
    }) %>% 
      bindEvent(input$thresh)
    
    observe({
      thresh <- 100 - as.numeric(input$thresh_pc)
      choices <- c(1, 5, 10, 20, thresh)
      updateSelectizeInput(session, "thresh", choices = choices, selected = isolate(thresh))
    }) %>% 
      bindEvent(input$thresh_pc)

    # Update threshold reactive values
    observe({
      fit <- fit_mod$fit_dist()
      thresh_type <- input$thresh_type
      req(fit)
      req(thresh_type)

      if (thresh_type != "Concentration") {
        conc <- input$conc
        req(conc)
        
        thresh <- signif(estimate_hp(fit, conc), 3)
        if (thresh < 1 | thresh > 99) {
          return()
        }
        thresh_rv$conc <- conc
        thresh_rv$percent <- thresh
      } else {
        thresh <- as.numeric(input$thresh)
        req(thresh)
        
        thresh_rv$percent <- thresh
        conc <- signif(estimate_hc(fit, thresh), 3)
        thresh_rv$conc <- conc
      }
    })

    # Predict hazard concentration
    predict_hc <- reactive({
      req(predict_trigger() > 0)
      req(main_nav() == "predict")
      fit <- fit_mod$fit_dist()
      req(fit)
      req(thresh_rv$percent)
      stats::predict(fit, proportion = unique(c(1:99, thresh_rv$percent)) / 100)
    }) %>%
      bindCache(
        thresh_rv$percent,
        thresh_rv$conc,
        fit_mod$fit_dist()
      ) %>%
      bindEvent(predict_trigger())
    
    # observe({print(predict_hc())})

    # Transformation
    transformation <- reactive({
      trans <- "log10"
      if (!input$xlog) {
        trans <- "identity"
      }
      trans
    })

    # Plot model average with xbreaks
    plot_model_average_xbreaks <- reactive({
      pred <- predict_hc()
      data <- data_mod$data()
      conc <- thresh_rv$conc
      percent <- thresh_rv$percent
      conc_col <- fit_mod$conc_column()
      req(pred)
      req(data)
      req(input$selectLabel)
      req(conc)
      req(conc_col)
      req(percent)
      
      conc_col <- make.names(conc_col)
      label_col <- ifelse(input$selectLabel == "-none-", NULL, make.names(input$selectLabel))

      gp <- ssdtools::ssd_plot(data,
        pred = pred,
        left = conc_col, label = label_col,
        hc = percent / 100
      )
      xbreaks <- gp_xbreaks(gp)
      xbreaks[xbreaks != conc]
    })

    # Main model average plot
    plot_model_average <- reactive({
      dat <- data_mod$data()
      pred <- predict_hc()
      conc_col <- fit_mod$conc_column()
      thresh_type <- input$thresh_type
      conc <- thresh_rv$conc
      perc <- thresh_rv$percent
      units <- fit_mod$units()
      
      req(input$thresh)
      req(input$selectColour)
      req(input$selectLabel)
      req(input$selectShape)
      req(conc_col)
      req(thresh_type)
      req(input$adjustLabel)
      req(conc)
      req(dat)
      
      colour <- if (input$selectColour == "-none-") {
        NULL
      } else {
        input$selectColour %>% make.names()
      }
      label <- if (input$selectLabel == "-none-") {
        NULL
      } else {
        input$selectLabel %>% make.names()
      }
      shape <- if (input$selectShape == "-none-") {
        NULL
      } else {
        input$selectShape %>% make.names()
      }
      percent <- if (!input$checkHc || is.null(perc)) {
        NULL
      } else {
        thresh_rv$percent
      }

      shape_data <- if (is.null(shape)) {
        NULL
      } else {
        dat[[shape]]
      }

      trans <- translations()
      
      validate(need(is.null(shape_data) | is.character(shape_data) | is.factor(shape_data),
                   message = tr("ui_hintsym", trans)))

      shift_label <- input$adjustLabel
      if (shift_label < 1) {
        shift_label <- 1
      }

      xmax <- NA
      if (!is.null(input$xMax)) {
        xmax <- input$xMax
      }

      xmin <- NA
      if (!is.null(input$xMin)) {
        xmin <- input$xMin
      }

      trans <- transformation()
      big.mark <- ","
      if (lang() == "French") {
        big.mark <- " "
      }

      silent_plot(plot_predictions(dat, pred,
        conc = conc_col, label = label, colour = colour,
        shape = shape, percent = percent, xbreaks = as.numeric(input$xbreaks),
        label_adjust = shift_label, xaxis = append_unit(input$xaxis, units),
        yaxis = input$yaxis, title = input$title, xmax = xmax, xmin = xmin,
        palette = input$selectPalette, legend_colour = input$legendColour,
        legend_shape = input$legendShape, trans = trans, text_size = input$size3,
        label_size = input$sizeLabel3, conc_value = thresh_rv$conc, big.mark = big.mark
      ))
    })
    
    # --- render predict results ----
    output$plotPred <- renderPlot({
      gp <- plot_model_average()
      gp
    })

    has_predict <- reactive({
      !is.null(predict_hc())
    }) %>%
      bindEvent(predict_hc())
    
    output$has_predict <- has_predict
    outputOptions(output, "has_predict", suspendWhenHidden = FALSE)
    
    has_cl <- reactive({
      !is.null(table_cl())
    }) %>%
      bindEvent(table_cl())
    
    output$has_cl <- has_cl
    outputOptions(output, "has_cl", suspendWhenHidden = FALSE)
    
    
    
    # Dynamic text outputs for HC/PC values
    output$hc_percent <- renderText({
      thresh_rv$percent
    })
    
    output$pc_percent <- renderText({
      100 - as.numeric(thresh_rv$percent %||% 0)
    })
    
    output$hc_conc <- renderText({
      thresh_rv$conc
    })
    
    output$clTable <- DT::renderDataTable({
      DT::datatable(table_cl(), options = list(dom = "t"))
    })
    
    table_cl <- reactive({
      dist <- fit_mod$fit_dist()
      waiter::waiter_show(html = waiting_screen_cl(), color = "#759dbe")
      nboot <- clean_nboot(input$bootSamp)
      if (input$thresh_type != "Concentration") {
        y <- ssd_hp_ave(dist, conc = thresh_rv$conc, nboot = nboot)
      } else {
        y <- ssd_hc_ave(dist, percent = thresh_rv$percent, nboot = nboot)
      }
      waiter::waiter_hide()
      y
    }) %>% 
      bindEvent(input$getCl)
    
    describe_cl <- reactive({
      trans <- translations()
      desc1 <- paste(tr("ui_3cldesc1", trans),
                     paste0("<b>", thresh_rv$percent, "</b>"))
      nboot <- clean_nboot(input$bootSamp)
      time <- estimate_time(nboot, lang())
      if (input$thresh_type != "Concentration") {
        desc1 <- paste(tr("ui_3cldesc11", trans),
                       paste0("<b>", thresh_rv$conc, "</b>"))
      }
      HTML(
        desc1,
        tr("ui_3cldesc2", trans),
        paste0("<b>", input$bootSamp, ".</b>"),
        "<br/>",
        tr("ui_3cldesc3", trans),
        paste0("<b>", time, "</b>"),
        tr("ui_3cldesc4", trans)
      )
    })
    
    describe_time <- reactive({
      trans <- translations()
      nboot <- clean_nboot(input$bootSamp)
      time <- estimate_time(nboot, lang())
     
      HTML(
        tr("ui_3cldesc3", trans),
        time,
        tr("ui_3cldesc4", trans)
      )
    })
    
    output$describeTime <- renderText({
      describe_time()
    })
    
    output$describeCl <- renderText({
      describe_cl()
    })
    
    
    
    # 
    # output$predDlPlot <- downloadHandler(
    #   filename = function() {
    #     "ssdtools_modelAveragePlot.png"
    #   },
    #   content = function(file) {
    #     ggplot2::ggsave(file,
    #                     plot = plot_model_average(), device = "png",
    #                     width = get_width(), height = get_height(), dpi = get_dpi()
    #     )
    #   }
    # )
    # 
    # output$predDlRds <- downloadHandler(
    #   filename = function() {
    #     "ssdtools_modelAveragePlot.rds"
    #   },
    #   content = function(file) {
    #     saveRDS(plot_model_average(), file = file)
    #   }
    # )
    # 
    # output$predDlTable <- downloadHandler(
    #   filename = function() {
    #     "ssdtools_predictTable.csv"
    #   },
    #   content <- function(file) {
    #     if (!is.null(table_cl())) {
    #       return(readr::write_csv(table_cl() %>% dplyr::as_tibble(), file))
    #     } else {
    #       return()
    #     }
    #   }
    # )
    # 
    # # Update shared values when predictions change
    # observe({
    #   shared_values$predictions <- predict_hc()
    #   shared_values$threshold_values <- list(
    #     percent = thresh_rv$percent,
    #     conc = thresh_rv$conc
    #   )
    #   shared_values$model_average_plot <- plot_model_average()
    # })
    # 
    waiting_screen_cl <- reactive({
      trans <- translations()
      tagList(
        waiter::spin_flower(),
        tagList(
          h3(paste(tr("ui_3cl", trans), "...")),
          br(),
          describe_cl()
        )
      )
    })
    # 
    # output$checkpred <- reactive({
    #   check_pred() != ""
    # })
    # outputOptions(output, "checkpred", suspendWhenHidden = FALSE)
    # 
    # output$showPredictResults <- reactive({
    #   return(show_predict_results())
    # })
    # outputOptions(output, "showPredictResults", suspendWhenHidden = FALSE)
    # 
    # 
    # output$hintPr <- renderText(hint(check_pred()))
    # 
    # 
    # # Return reactive values for use by other modules
    # return(
    #   list(
    #     predictions = reactive({ predict_hc() }),
    #     plot_model_average = reactive({ plot_model_average() }),
    #     check_pred = reactive({ check_pred() }),
    #     threshold_values = reactive({ 
    #       list(percent = thresh_rv$percent, conc = thresh_rv$conc)
    #     }),
    #     show_predict_results = reactive({ 
    #       !is.null(predict_hc()) && !inherits(predict_hc(), "try-error")
    #     })
    #   )
    # )
  })
}