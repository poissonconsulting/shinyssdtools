# R Code Module UI
mod_rcode_ui <- function(id) {
  ns <- NS(id)
  
  div(
    style = "padding: 1rem;",
    card(
      card_header(uiOutput("ui_nav4")), 
      card_body(
        tagList(
          uiOutput(ns("ui_4help")),
          div(
            id = "codes",
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; font-family: 'Courier New', monospace; font-size: 12px;",
            uiOutput(ns("codeHead")),
            br(),
            uiOutput(ns("codeData")),
            br(),
            uiOutput(ns("codeFit")),
            br(),
            uiOutput(ns("codeSaveFit")),
            br(),
            uiOutput(ns("codePredPlot")),
            br(),
            uiOutput(ns("codeSavePred")),
            br(),
            uiOutput(ns("codePredCl"))
          )
        )
      )
    )
  )
}

# R Code Module Server
mod_rcode_server <- function(id, shared_values, translations) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper functions for code generation
    code_label <- reactive({
      label_val <- shared_values$predict_label
      if (is.null(label_val) || label_val == "-none-") {
        return("NULL")
      }
      paste0("'", label_val %>% make.names(), "'")
    })
    
    code_colour <- reactive({
      colour_val <- shared_values$predict_colour
      if (is.null(colour_val) || colour_val == "-none-") {
        return("NULL")
      }
      paste0("'", colour_val %>% make.names(), "'")
    })
    
    code_shape <- reactive({
      shape_val <- shared_values$predict_shape
      if (is.null(shape_val) || shape_val == "-none-") {
        return("NULL")
      }
      paste0("'", shape_val %>% make.names(), "'")
    })
    
    code_hc <- reactive({
      threshold_vals <- shared_values$threshold_values
      show_hc <- shared_values$show_hc
      if (is.null(show_hc) || !show_hc || is.null(threshold_vals)) {
        return("NULL")
      }
      threshold_vals$percent / 100
    })
    
    # Get plot dimensions
    get_width <- reactive({
      width_val <- shared_values$plot_width
      ifelse(is.null(width_val) || width_val == 0, 6, width_val)
    })
    
    get_width2 <- reactive({
      width_val <- shared_values$fit_plot_width
      ifelse(is.null(width_val) || width_val == 0, 6, width_val)
    })
    
    get_height <- reactive({
      height_val <- shared_values$plot_height
      ifelse(is.null(height_val) || height_val == 0, 4, height_val)
    })
    
    get_height2 <- reactive({
      height_val <- shared_values$fit_plot_height
      ifelse(is.null(height_val) || height_val == 0, 4, height_val)
    })
    
    get_dpi <- reactive({
      dpi_val <- shared_values$plot_dpi
      if (is.null(dpi_val)) dpi_val <- 300
      if (dpi_val > 3000) return(3000)
      if (dpi_val == 0) return(300)
      dpi_val
    })
    
    get_dpi2 <- reactive({
      dpi_val <- shared_values$fit_plot_dpi
      if (is.null(dpi_val)) dpi_val <- 300
      if (dpi_val > 2000) return(2000)
      if (dpi_val < 50) return(50)
      dpi_val
    })
    
    # Code section outputs
    output$ui_4help <- renderUI({
      trans_obj <- translations()
      HTML(tr("ui_4help", trans_obj))
    })
    
    # render code 
    output$codeHead <- renderUI({
      if (upload.values$upload_state == "hot" && is.na(read_data()$Concentration[1])) {
        return()
      }
      l1 <- "install.packages('ssdtools')"
      l2 <- "library(ssdtools)"
      l3 <- "library(ggplot2)"
      l4 <- "library(dplyr)"
      if (upload.values$upload_state == "upload") {
        l5 <- "library(readr)"
      } else {
        l5 <- NULL
      }
      HTML(paste(l1, l2, l3, l4, l5, sep = "<br/>"))
    })
    
    output$codeData <- renderUI({
      hot <- paste0("data <- ", utils::capture.output(dput(clean_data())) %>% glue::glue_collapse())
      upload <- paste0("data <- read_csv(file = '", input$uploadData$name, "')")
      demo <- "data <- ssddata::ccme_boron"
      name <- "colnames(data) <- make.names(colnames(data))"
      if (upload.values$upload_state == "hot") {
        return(HTML(paste(hot, name, sep = "<br/>")))
      }
      if (upload.values$upload_state == "upload") {
        return(HTML(paste(upload, name, sep = "<br/>")))
      }
      if (upload.values$upload_state == "demo") {
        return(HTML(paste(demo, name, sep = "<br/>")))
      }
    })
    
    output$codeFit <- renderUI({
      req(check_fit() == "")
      ylab <- input$yaxis2
      xlab <- input$xaxis2
      text_size <- input$size2
      fit <- paste0(
        "dist <- ssd_fit_dists(data, left = '",
        input$selectConc %>% make.names(),
        "', dists = c(",
        paste0("'", input$selectDist, "'", collapse = ", "), ")",
        ", silent = TRUE, reweight = FALSE",
        ", rescale = ", input$rescale, ")"
      )
      plot <- paste0(
        "ssd_plot_cdf(dist, ylab = '", ylab, "', xlab = '", xlab,
        "', delta = Inf, <br/>average = NA, theme_classic = TRUE, text_size = ",
        text_size, ") <br/>"
      )
      
      table <- "ssd_gof(dist) %>% dplyr::mutate_if(is.numeric, ~ signif(., 3))"
      HTML(paste(fit, plot, table, sep = "<br/>"))
    })
    
    output$codePredPlot <- renderUI({
      req(check_fit() == "")
      req(check_pred() == "")
      req(input$selectLabel)
      xmax <- input$xMax
      xmin <- input$xMin
      xlimits <- ifelse(is.na(xmin) & is.na(xmax), "NULL", paste0("c(", xmin, ", ", xmax, ")"))
      legend.colour <- ifelse(is.null(input$legendColour) || input$legendColour == "-none-", "NULL", paste0("'", input$legendColour, "'"))
      legend.shape <- ifelse(is.null(input$legendShape) || input$legendShape == "-none-", "NULL", paste0("'", input$legendShape, "'"))
      text_size <- input$size3
      xlab <- input$xaxis
      ylab <- input$yaxis
      title <- input$title
      big.mark <- ifelse(translation.value$lang == "French", " ", ",")
      trans <- transformation()
      xbreaks <- input$xbreaks
      xbreaks <- paste0("c(", paste(xbreaks, collapse = ", "), ")")
      pred <- paste0("pred <- predict(dist, proportion = unique(c(1:99, ", thresh_rv$percent, ")/100))")
      plot <- paste0(
        "ssd_plot(data, pred, left = '", make.names(input$selectConc),
        "', label = ", code_label(),
        ", shape = ", code_shape(),
        ", color = ", code_colour(),
        ",  <br/>label_size = ", input$sizeLabel3,
        ", ylab = '", ylab,
        "', xlab = '", xlab,
        "', ci = FALSE, shift_x = ", input$adjustLabel,
        ", hc = ", code_hc(),
        ", <br/>big.mark = '", big.mark,
        "', trans = '", trans,
        "', xlimits = ", xlimits,
        ", xbreaks = ", xbreaks,
        ", text_size = ", text_size,
        ", theme_classic = TRUE",
        ") + <br/> ggtitle('", title,
        "') + <br/>scale_color_brewer(palette = '", input$selectPalette, "', name = ", legend.colour, ") +<br/>
                     scale_shape(name = ", legend.shape, ")"
      )
      HTML(paste(pred, plot, sep = "<br/>"))
    })
    
    output$codePredCl <- renderUI({
      req(input$getCl)
      req(check_fit() == "")
      req(check_pred() == "")
      form <- "ssd_hc"
      arg <- "proportion"
      thresh <- thresh_rv$percent / 100
      if (input$thresh_type != "Concentration") {
        form <- "ssd_hp"
        arg <- "conc"
        thresh <- thresh_rv$conc
      }
      
      conf <- paste0(
        paste0(form, "(dist, ", arg, " = "), thresh, ", ci = TRUE",
        ", nboot = ", input$bootSamp %>% gsub(",", "", .) %>% as.integer(), "L, min_pboot = 0.8)"
      )
      conf2 <- paste0(
        paste0(form, "(dist, ", arg, " = "), thresh, ", ci = TRUE, average = FALSE",
        ", nboot = ", input$bootSamp %>% gsub(",", "", .) %>% as.integer(), "L, min_pboot = 0.8)"
      )
      bind <- paste0("dplyr::bind_rows(", conf, ", ", conf2, ")")
      HTML(paste(bind, sep = "<br/>"))
    })
    
    output$codeSaveFit <- renderUI({
      req(check_fit() == "")
      save <- paste0(
        "ggsave('fit_dist_plot.png',
                    width = ", get_width2(),
        " , height = ", get_height2(),
        " , dpi = ", get_dpi2(),
        ")"
      )
      HTML(paste(save, sep = "<br/>"))
    })
    
    output$codeSavePred <- renderUI({
      req(check_fit() == "")
      req(check_pred() == "")
      req(input$selectLabel)
      save <- paste0(
        "ggsave('model_average_plot.png',
                    width = ", get_width(),
        " , height = ", get_height(),
        " , dpi = ", get_dpi(),
        ")"
      )
      HTML(paste(save, sep = "<br/>"))
    })
    
    # Return reactive indicator
    return(
      list(
        has_code = reactive({ 
          !is.null(shared_values$fitted_dist) && !is.null(shared_values$data)
        })
      )
    )
  })
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x