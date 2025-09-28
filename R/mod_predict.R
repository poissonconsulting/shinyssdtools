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
          width = 375,
          style = "height: calc(100vh - 150px); overflow-y: auto; overflow-x: hidden;",
          
          bslib::accordion(
            open = c("hc_pred", "cl_pred"),
# ui thresh ---------------------------------------------------------------
            accordion_panel(
              title = span(`data-translate` = "ui_3est", "Estimate hazard concentration"),
              value = "hc_pred",
              radioButtons(ns("threshType"), 
                           label = span(`data-translate` = "ui_3threshlabel", "Threshold type"),
                           choices = c("Concentration" = "Concentration", "Fraction affected" = "Fraction"),
                           selected = "Concentration", inline = TRUE
              ),
              conditionalPanel(
                condition = glue::glue("input['{ns(\"threshType\")}'] != 'Concentration'"),
                layout_column_wrap(
                  width = 1/2,
                  numericInput(ns("conc"),
                               label = span(`data-translate` = "ui_3byconc", "by concentration"),
                               value = 1, min = 0,
                               max = 100, step = 0.1
                  )
                )
              ),
              conditionalPanel(
                condition = glue::glue("input['{ns(\"threshType\")}'] == 'Concentration'"),
                layout_column_wrap(
                  width = 1/2,
                  selectizeInput(ns("thresh"),
                                 label = span(`data-translate` = "ui_3affecting", "affecting % species"),
                                 choices = c(1, 5, 10, 20),
                                 options = list(create = TRUE, createFilter = "^[1-9][0-9]?$|^99$"),
                                 selected = 5
                  ),
                  selectizeInput(ns("threshPc"),
                                 label = span(`data-translate` = "ui_3protecting", "protecting % species"),
                                 choices = c(99, 95, 90, 80),
                                 options = list(create = TRUE, createFilter = "^[1-9][0-9]?$|^99$"),
                                 selected = 95
                  )
                )
              )
              # uiOutput(ns("uiThresh"))
            ),
# ui cl -------------------------------------------------------------------
          accordion_panel(
            title = div(
              span(`data-translate` = "ui_3cl", "Get confidence limits"),
              bslib::tooltip(
                bsicons::bs_icon("question-circle", style = "margin-left: 0.5rem; color: #6c757d; outline: none; border: none;"),
                span(`data-translate` = "ui_3help", "Click 'Get CL' to calculate the upper and lower confidence limits (CL) for the estimate."),
                placement = "right"
              )
            ),
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
            shiny::helpText( htmlOutput(ns("describeTime"))),
          ),
# ui plot formatting -------------------------------------------------------
            bslib::accordion_panel(
              title = span(`data-translate` = "ui_3plotopts", "Plot formatting options"),
              value = "plot_format_pred",
              selected = FALSE, 
              static_label_input(ns("selectLabel"), "ui_3label", "Label by:", ns("uiSelectLabel")),
              static_label_input(ns("selectColour"), "ui_3colour", "Colour by:", ns("uiSelectColour")),
              static_label_input(ns("selectShape"), "ui_3symbol", "Symbol by:", ns("uiSelectShape")),
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
              static_label_input(ns("legendColour"), "ui_3legend", "Legend colour", ns("uiLegendColour")),
              static_label_input(ns("legendShape"), "ui_3shape", "Legend shape", ns("uiLegendShape")),
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
              static_label_input(ns("xbreaks"), "ui_xbreaks", "X breaks", ns("uiXbreaks"))
            )
            ),
        ),
# ui outputs --------------------------------------------------------------
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
                        condition = glue::glue("input['{ns(\"threshType\")}'] == 'Concentration'"),
                        div(
                          span("HC"), 
                          textOutput(ns("hcPercent"), inline = TRUE),
                          span("/ PC"), 
                          textOutput(ns("pcPercent"), inline = TRUE),
                          span(": "), 
                          tags$b(textOutput(ns("hcConc"), inline = TRUE))
                        ),
                        div(
                          span(`data-translate` = "ui_3hc", "The model averaged estimate of the concentration that affects "),
                          tags$b(textOutput(ns("estPerc"), inline = TRUE)),
                          span(`data-translate` = "ui_3hc2", " % of species is "),
                          tags$b(textOutput(ns("estConc"), inline = TRUE))
                        )
                      ),
                      conditionalPanel(
                        condition = glue::glue("input['{ns(\"threshType\")}'] != 'Concentration'"),
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
                    span(`data-translate` = "ui_3cl2", "Confidence Limits")
                  )
                ),
                card_body(padding = 25,
                  ui_download_popover_table(tab = "pred", ns = ns),
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
    
    # trigger for updating predictions - only occur when on predict tab
    predict_trigger <- reactiveVal(0)
    
    observe({
      if(main_nav() == "predict") {
        current_val <- isolate(predict_trigger())
        predict_trigger(current_val + 1)
      }
    }) %>%
      bindEvent(main_nav())
    
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
      frac <- tr("ui_3thresh", trans)
      choices <- c(
        "Concentration" = "Concentration",
        setNames("Fraction", frac)
      )
      updateRadioButtons(session, "threshType", 
                         choices = choices,
                         selected = input$threshType)
    }) %>%
      bindEvent(translations())
    
    observe({
      current <- lang()
      choices <- c("500", "1,000", "5,000", "10,000")
      if(current == "french"){
        choices <- c("500", "1 000", "5 000", "10 000")
      }
      updateSelectInput(session, "bootSamp", 
                        choices = choices, 
                        selected = choices[2])
    }) %>%
      bindEvent(lang())
    
    thresh_rv <- reactiveValues(
      percent = NULL,
      conc = NULL
    )
    
    column_names <- reactive({
      names(data_mod$clean_data())
    })
    
# validation --------------------------------------------------------------
    iv <- InputValidator$new()
    
    iv$add_rule("conc", function(value) {
      trans <- translations()
      
      if(is.null(value)){
        return(NULL)
      }
      if(is.na(value)){
        return(as.character(tr("ui_hintconcmiss", trans)[1]))
      }
      if(value == 0){
        return(as.character(tr("ui_hintconc0", trans)[1]))
      }
      NULL  
    })
    
    iv$add_rule("selectColour", function(value) {
      if(is.null(value)){
        return(NULL)
      }
      trans <- translations()
      dat <- data_mod$data()
      
      colour_data <- dat[[value]]
      
      if (is.numeric(colour_data)) {
        return(as.character(tr("ui_hintcolour", trans)[1]))
      }
      NULL  
    })
    
    iv$add_rule("selectShape", function(value) {
      if(is.null(value)){
        return(NULL)
      }
      trans <- translations()
      dat <- data_mod$data()
      
      sym_data <- dat[[value]]
      
      if (is.numeric(sym_data)) {
        return(as.character(tr("ui_hintsym", trans)[1]))
      }
      NULL  
    })
    
    iv$enable()
     
# ui renders ----------------------------------------------------------
    output$uiSelectLabel <- renderUI({
      cols <- column_names()
      selectInput(ns("selectLabel"),
        label = NULL,
        choices = c("-none-", cols),
        selected = guess_sp(cols)
      )
    })
    
    output$uiSelectColour <- renderUI({
      selectInput(ns("selectColour"),
        label = NULL,
        choices = c("-none-", column_names()),
        selected = "-none-"
      )
    })
    
    output$uiSelectShape <- renderUI({
      selectInput(ns("selectShape"),
        label = NULL,
        choices = c("-none-", column_names()),
        selected = "-none-"
      )
    })
    
    output$uiLegendColour <- renderUI({
      textInput(ns("legendColour"), 
                label = NULL, 
                value = input$selectColour)
    })
    
    output$uiLegendShape <- renderUI({
      textInput(ns("legendShape"), 
                label = NULL, 
                value = input$selectShape)
    })
    
    output$uiXbreaks <- renderUI({
      xbreaks <- plot_model_average_xbreaks()
      selectizeInput(ns("xbreaks"),
                     label = NULL,
                     options = list(create = TRUE, plugins = list("remove_button")),
                     choices = xbreaks,
                     selected = xbreaks,
                     multiple = TRUE
      )
    })
    
    outputOptions(output, "uiXbreaks", suspendWhenHidden = FALSE)
    outputOptions(output, "uiLegendColour", suspendWhenHidden = FALSE)
    outputOptions(output, "uiLegendShape", suspendWhenHidden = FALSE)
    outputOptions(output, "uiSelectLabel", suspendWhenHidden = FALSE)
    outputOptions(output, "uiSelectColour", suspendWhenHidden = FALSE)
    outputOptions(output, "uiSelectShape", suspendWhenHidden = FALSE)
    

# output renders ----------------------------------------------------------
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
    
    output$describeTime <- renderText({
      describe_time()
    })
    
    output$describeCl <- renderText({
      describe_cl()
    })
    
    output$plotPred <- renderPlot({
      gp <- plot_model_average()
      silent_plot(gp)
    })
    
    # Dynamic text outputs for HC/PC values
    output$hcPercent <- renderText({
      thresh_rv$percent
    })
    
    output$pcPercent <- renderText({
      100 - as.numeric(thresh_rv$percent %||% 0)
    })
    
    output$hcConc <- renderText({
      thresh_rv$conc
    })
    
    output$clTable <- DT::renderDataTable({
      DT::datatable(table_cl(), options = list(dom = "t"))
    })
  
# update thresh RVs --------------------------------------------------------
    observe({
      thresh_pc <- 100 - as.numeric(input$thresh)
      choices <- unique(c(99, 95, 90, 80, thresh_pc))
      updateSelectizeInput(session, "threshPc", choices = choices, selected = isolate(thresh_pc))
    }) %>% 
      bindEvent(input$thresh)
    
    observe({
      thresh <- 100 - as.numeric(input$threshPc)
      choices <- c(1, 5, 10, 20, thresh)
      updateSelectizeInput(session, "thresh", choices = choices, selected = isolate(thresh))
    }) %>% 
      bindEvent(input$threshPc)

    # Update threshold reactive values
    observe({
      fit <- fit_mod$fit_dist()
      thresh_type <- input$threshType
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

# reactives ---------------------------------------------------------------
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
    
    transformation <- reactive({
      trans <- "log10"
      if (!input$xlog) {
        trans <- "identity"
      }
      trans
    })

    plot_model_average_xbreaks <- reactive({
      pred <- predict_hc()
      dat <- data_mod$data()
      conc <- thresh_rv$conc
      percent <- thresh_rv$percent
      conc_col <- fit_mod$conc_column()
      req(pred)
      req(dat)
      req(input$selectLabel)
      req(conc)
      req(conc_col)
      req(percent)
      
      conc_col <- make.names(conc_col)
      label_col <- ifelse(input$selectLabel == "-none-", NULL, make.names(input$selectLabel))

      if(label_col %in% names(dat) & conc_col %in% names(dat)){
        gp <- safe_try(ssdtools::ssd_plot(dat,
                                          pred = pred,
                                          left = conc_col, label = label_col,
                                          hc = percent / 100
        )) 
        xbreaks <- gp_xbreaks(gp)
        return(xbreaks[xbreaks != conc])
      } 
      NULL
    })

    plot_model_average <- reactive({
      dat <- data_mod$data()
      pred <- predict_hc()
      conc_col <- fit_mod$conc_column()
      thresh_type <- input$threshType
      conc <- thresh_rv$conc
      perc <- thresh_rv$percent
      units <- fit_mod$units()
      trans <- translations()
      
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
      
      x <- safe_try(plot_predictions(dat, pred,
        conc = conc_col, label = label, colour = colour,
        shape = shape, percent = percent, xbreaks = as.numeric(input$xbreaks),
        label_adjust = shift_label, xaxis = append_unit(input$xaxis, units),
        yaxis = input$yaxis, title = input$title, xmax = xmax, xmin = xmin,
        palette = input$selectPalette, legend_colour = input$legendColour,
        legend_shape = input$legendShape, trans = trans, text_size = input$size3,
        label_size = input$sizeLabel3, conc_value = thresh_rv$conc, big.mark = big.mark
      ))
      if(!is.null(x)){
        return(x)
      } else(
        NULL
      )
    })
    
    table_cl <- reactive({
      dist <- fit_mod$fit_dist()
      waiter::waiter_show(html = waiting_screen_cl(), color = "#759dbe")
      nboot <- clean_nboot(input$bootSamp)
      if (input$threshType != "Concentration") {
        y <- ssd_hp_ave(dist, conc = thresh_rv$conc, nboot = nboot)
      } else {
        y <- ssd_hc_ave(dist, percent = thresh_rv$percent, nboot = nboot)
      }
      y$dists <- NULL
      y$samples <- NULL
      waiter::waiter_hide()
      y
    }) %>% 
      bindCache(thresh_rv$percent, thresh_rv$conc, input$bootSamp) %>% 
      bindEvent(input$getCl)
    
    describe_cl <- reactive({
      trans <- translations()
      desc1 <- paste(tr("ui_3cldesc1", trans),
                     paste0("<b>", thresh_rv$percent, "</b>"))
      nboot <- clean_nboot(input$bootSamp)
      time <- estimate_time(nboot, lang())
      if (input$threshType != "Concentration") {
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
    
# downloaders -------------------------------------------------------------
    output$predDlPlot <- downloadHandler(
      filename = function() {
        "ssdtools_model_average_plot.png"
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot_model_average(), 
                        device = "png",
                        width = input$width, 
                        height = input$height, 
                        dpi = input$dpi
        )
      }
    )
    
    output$predDlRds <- downloadHandler(
      filename = function() {
        "ssdtools_model_average_plot.rds"
      },
      content = function(file) {
        saveRDS(plot_model_average(), file = file)
      }
    )
    
    output$predDlCsv <- downloadHandler(
      filename = function() {
        "ssdtools_cl_table.csv"
      },
      content = function(file) {
        readr::write_csv(dplyr::as_tibble(table_cl()), file)
      }
    )
    
    output$predDlXlsx <- downloadHandler(
      filename = function() {
        "ssdtools_cl_table.xlsx"
      },
      content = function(file) {
        writexl::write_xlsx(dplyr::as_tibble(table_cl()), file)
      }
    )

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
   
    return(
      list(
        predictions = predict_hc,
        model_average_plot = plot_model_average,
        predict_cl = table_cl,
        nboot = reactive({input$bootSamp}),
        threshold_values = reactive({
          list(percent = thresh_rv$percent, conc = thresh_rv$conc)
        }),
        threshold_type = reactive({input$threshType}),
        select_label = reactive({input$selectLabel}),
        select_colour = reactive({input$selectColour}),
        select_shape = reactive({input$selectShape}),
        legend_colour = reactive({input$legendColour}),
        legend_shape = reactive({input$legendShape}),
        x_min = reactive({input$xMin}),
        x_max = reactive({input$xMax}),
        text_size = reactive({input$size3}),
        label_size = reactive({input$sizeLabel3}),
        xaxis_label = reactive({input$xaxis}),
        yaxis_label = reactive({input$yaxis}),
        title = reactive({input$title}),
        palette = reactive({input$selectPalette}),
        adjust_label = reactive({input$adjustLabel}),
        xbreaks = reactive({input$xbreaks}),
        check_hc = reactive({input$checkHc}),
        x_log = reactive({input$xlog}),
        has_cl = has_cl,
        has_predict = has_predict
      )
    )
  })
}