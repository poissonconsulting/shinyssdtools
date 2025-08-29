# Predict Module UI
mod_predict_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    padding = "1rem",
    gap = "1rem",
    sidebar = sidebar(
      width = 350,
      radioButtons(ns("thresh_type"), 
                   label = span(`data-translate` = "ui_3threshlabel", "Threshold type"),
                   choices = c("Concentration", span(`data-translate` = "ui_3thresh", "Fraction affected")),
                   selected = "Concentration", inline = TRUE
      ),
      uiOutput(ns("ui_3thresh")),
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
    ),
    conditionalPanel(
      condition = "input.main_nav == 'predict'",
      div(
        class = "p-3",
        conditionalPanel(condition = "output.checkpred", htmlOutput("hintPr")),
        conditionalPanel(
          condition = "output.showPredictResults",
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span(`data-translate` = "ui_3model", "Model Average Plot"),
              ui_download_popover(tab = "pred")
            ),
            card_body(
              plotOutput("modelAveragePlot"),
              conditionalPanel(condition = "output.showPredictResults", htmlOutput("estHc"))
            )
          )
        ),
        conditionalPanel(
          condition = "output.showPredictResults",
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              div(
                span(`data-translate` = "ui_3cl", "Confidence Limits"),
                bslib::tooltip(
                  bsicons::bs_icon("question-circle", style = "margin-left: 0.5rem; color: #6c757d; outline: none; border: none;"),
                  uiOutput("ui_3help"),
                  placement = "right"
                )
              ),
              downloadButton(
                "dlPredTable",
                label = tagList(
                  bsicons::bs_icon("download"),
                  span(`data-translate` = "ui_2download", "Download")
                ),
                icon = NULL,
                style = "padding:4px; font-size:80%"
              )
            ),
            card_body(
              div(class = "d-flex gap-4 align-items-start mb-3", div(
                selectInput(
                  "bootSamp",
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
                )
              ), div(
                actionButton(
                  "getCl",
                  label = span(`data-translate` = "ui_3clbutton", "Get CL"),
                  class = "btn-primary",
                  style = "white-space: nowrap;"
                )
              )),
              div(class = "mb-3", htmlOutput("describeCl")),
              # Results table
              conditionalPanel(condition = "output.showPredictResults", DT::dataTableOutput("clTable"))
            )
          )
        )
      )
    ))
}

# Predict Module Server
mod_predict_server <- function(id, shared_values, translations) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Threshold reactive values
    thresh_rv <- reactiveValues(
      percent = NULL,
      conc = NULL
    )
    
    # Column names from shared values
    column_names <- reactive({
      req(shared_values$column_names)
      shared_values$column_names
    })
    
    # Render UI components based on data
    output$selectLabel <- renderUI({
      cols <- column_names()
      selectInput(ns("selectLabel"),
        label = span(`data-translate` = "ui_3label", "Label"),
        choices = c("-none-", cols),
        selected = guess_spp(cols)
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
    
    output$ui_3thresh <- renderUI({
      req(input$thresh_type)
      if (input$thresh_type != "Concentration") {
        return(numericInput(ns("conc"),
          label = span(`data-translate` = "ui_3byconc", "By concentration"),
          value = 1, min = 0,
          max = 100, step = 0.1, width = "100px"
        ))
      }
      div(
        inline(selectizeInput(ns("thresh"),
          label = span(`data-translate` = "ui_3affecting", "% affecting"),
          choices = c(1, 5, 10, 20),
          options = list(create = TRUE, createFilter = "^[1-9][0-9]?$|^99$"),
          selected = 5, width = "100px"
        )),
        inline(selectizeInput(ns("thresh_pc"),
          label = span(`data-translate` = "ui_3protecting", "% protecting"),
          choices = c(99, 95, 90, 80),
          options = list(create = TRUE, createFilter = "^[1-9][0-9]?$|^99$"),
          selected = 95, width = "100px"
        ))
      )
    })
  
    
    # Threshold logic observers
    observeEvent(input$thresh, {
      thresh_pc <- 100 - as.numeric(input$thresh)
      choices <- unique(c(99, 95, 90, 80, thresh_pc))
      updateSelectizeInput(session, "thresh_pc", choices = choices, selected = isolate(thresh_pc))
    })
    
    observeEvent(input$thresh_pc, {
      thresh <- 100 - as.numeric(input$thresh_pc)
      choices <- c(1, 5, 10, 20, thresh)
      updateSelectizeInput(session, "thresh", choices = choices, selected = isolate(thresh))
    })
    
    # Update threshold reactive values
    observe({
      req(shared_values$fitted_dist)
      req(input$thresh_type)
      x <- shared_values$fitted_dist
      
      if (input$thresh_type != "Concentration") {
        req(input$conc)
        conc <- input$conc
        thresh <- signif(estimate_hp(x, conc), 3)
        if (thresh < 1 | thresh > 99) {
          return()
        }
        thresh_rv$conc <- conc
        thresh_rv$percent <- thresh
      } else {
        req(input$thresh)
        thresh <- as.numeric(input$thresh)
        thresh_rv$percent <- thresh
        conc <- signif(estimate_hc(x, thresh), 3)
        thresh_rv$conc <- conc
      }
    })
    
    # Check prediction validation
    check_pred <- reactive({
      req(shared_values$data)
      data <- shared_values$data
      trans_obj <- translations()
      
      if ("Concentration" %in% names(data) && length(data[["Concentration"]]) == 0L) {
        return(tr("ui_hintdata", trans_obj))
      }
      if (is.null(shared_values$selected_conc)) {
        return(tr("ui_hintpred", trans_obj))
      }
      # Add fit check here if needed
      ""
    })
    
    # Predict hazard concentration
    predict_hc <- reactive({
      req(shared_values$fitted_dist)
      req(thresh_rv$percent)
      dist <- shared_values$fitted_dist
      stats::predict(dist, proportion = unique(c(1:99, thresh_rv$percent)) / 100)
    })
    
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
      req(predict_hc())
      req(shared_values$data)
      req(shared_values$selected_conc)
      req(input$selectLabel)
      req(thresh_rv$conc)
      
      pred <- predict_hc()
      data <- shared_values$data
      conc <- thresh_rv$conc
      percent <- thresh_rv$percent
      conc_col <- make.names(shared_values$selected_conc)
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
      req(input$thresh)
      req(input$selectColour)
      req(input$selectLabel)
      req(input$selectShape)
      req(shared_values$selected_conc)
      req(input$thresh_type)
      req(input$adjustLabel)
      req(thresh_rv$percent)
      req(thresh_rv$conc)
      req(shared_values$data)
      req(shared_values$selected_unit)
      
      data <- shared_values$data
      pred <- predict_hc()
      conc <- shared_values$selected_conc %>% make.names()
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
      percent <- if (!input$checkHc || is.null(thresh_rv$percent)) {
        NULL
      } else {
        thresh_rv$percent
      }
      
      shape_data <- if (is.null(shape)) {
        NULL
      } else {
        data[[shape]]
      }
      
      trans_obj <- translations()
      validate(need(is.null(shape_data) | is.character(shape_data) | is.factor(shape_data), 
                   message = tr("ui_hintsym", trans_obj)))
      
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
      if (shared_values$current_language == "French") {
        big.mark <- " "
      }
      
      silent_plot(plot_predictions(data, pred,
        conc = conc, label = label, colour = colour,
        shape = shape, percent = percent, xbreaks = as.numeric(input$xbreaks),
        label_adjust = shift_label, xaxis = append_unit(input$xaxis, shared_values$selected_unit),
        yaxis = input$yaxis, title = input$title, xmax = xmax, xmin = xmin,
        palette = input$selectPalette, legend_colour = input$legendColour,
        legend_shape = input$legendShape, trans = trans, text_size = input$size3,
        label_size = input$sizeLabel3, conc_value = thresh_rv$conc, big.mark = big.mark
      ))
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
    
    output$predDlPlot <- downloadHandler(
      filename = function() {
        "ssdtools_modelAveragePlot.png"
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = plot_model_average(), device = "png",
                        width = get_width(), height = get_height(), dpi = get_dpi()
        )
      }
    )
    
    output$predDlRds <- downloadHandler(
      filename = function() {
        "ssdtools_modelAveragePlot.rds"
      },
      content = function(file) {
        saveRDS(plot_model_average(), file = file)
      }
    )
    
    output$predDlTable <- downloadHandler(
      filename = function() {
        "ssdtools_predictTable.csv"
      },
      content <- function(file) {
        if (!is.null(table_cl())) {
          return(readr::write_csv(table_cl() %>% dplyr::as_tibble(), file))
        } else {
          return()
        }
      }
    )
    
    # Update shared values when predictions change
    observe({
      shared_values$predictions <- predict_hc()
      shared_values$threshold_values <- list(
        percent = thresh_rv$percent,
        conc = thresh_rv$conc
      )
      shared_values$model_average_plot <- plot_model_average()
    })
    
    waiting_screen_cl <- reactive({
      tagList(
        waiter::spin_flower(),
        tagList(
          h3(paste(tr("ui_3cl", trans()), "...")),
          br(),
          describe_cl()
        )
      )
    })
    
    output$checkpred <- reactive({
      check_pred() != ""
    })
    outputOptions(output, "checkpred", suspendWhenHidden = FALSE)
    
    output$showPredictResults <- reactive({
      return(show_predict_results())
    })
    outputOptions(output, "showPredictResults", suspendWhenHidden = FALSE)
    
    
    output$hintPr <- renderText(hint(check_pred()))
    
    
    # Return reactive values for use by other modules
    return(
      list(
        predictions = reactive({ predict_hc() }),
        plot_model_average = reactive({ plot_model_average() }),
        check_pred = reactive({ check_pred() }),
        threshold_values = reactive({ 
          list(percent = thresh_rv$percent, conc = thresh_rv$conc)
        }),
        show_predict_results = reactive({ 
          !is.null(predict_hc()) && !inherits(predict_hc(), "try-error")
        })
      )
    )
  })
}