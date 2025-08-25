# R Code Module UI
mod_rcode_ui <- function(id) {
  ns <- NS(id)
  
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
    
    output$codeHead <- renderUI({
      if (shared_values$data_source == "hot" && is.null(shared_values$data)) {
        return()
      }
      
      l1 <- "install.packages('ssdtools')"
      l2 <- "library(ssdtools)"
      l3 <- "library(ggplot2)"
      l4 <- "library(dplyr)"
      if (shared_values$data_source == "upload") {
        l5 <- "library(readr)"
      } else {
        l5 <- NULL
      }
      HTML(paste(l1, l2, l3, l4, l5, sep = "<br/>"))
    })
    
    output$codeData <- renderUI({
      req(shared_values$data)
      
      hot <- paste0("data <- ", utils::capture.output(dput(shared_values$data)) %>% glue::glue_collapse())
      upload <- paste0("data <- read_csv(file = '", shared_values$upload_filename, "')")
      demo <- "data <- ssddata::ccme_boron"
      name <- "colnames(data) <- make.names(colnames(data))"
      
      if (shared_values$data_source == "hot") {
        return(HTML(paste(hot, name, sep = "<br/>")))
      }
      if (shared_values$data_source == "upload") {
        return(HTML(paste(upload, name, sep = "<br/>")))
      }
      if (shared_values$data_source == "demo") {
        return(HTML(paste(demo, name, sep = "<br/>")))
      }
    })
    
    output$codeFit <- renderUI({
      req(shared_values$fitted_dist)
      req(shared_values$selected_conc)
      req(shared_values$selected_dists)
      
      ylab <- shared_values$fit_yaxis %||% "Percent"
      xlab <- shared_values$fit_xaxis %||% "Concentration"
      text_size <- shared_values$fit_text_size %||% 12
      rescale <- shared_values$rescale %||% FALSE
      
      fit <- paste0(
        "dist <- ssd_fit_dists(data, left = '",
        shared_values$selected_conc %>% make.names(),
        "', dists = c(",
        paste0("'", shared_values$selected_dists, "'", collapse = ", "), ")",
        ", silent = TRUE, reweight = FALSE",
        ", rescale = ", rescale, ")"
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
      req(shared_values$fitted_dist)
      req(shared_values$predictions)
      req(shared_values$threshold_values)
      
      threshold_vals <- shared_values$threshold_values
      xmax <- shared_values$predict_xmax
      xmin <- shared_values$predict_xmin
      xlimits <- ifelse(is.na(xmin) & is.na(xmax), "NULL", paste0("c(", xmin, ", ", xmax, ")"))
      legend_colour <- shared_values$legend_colour %||% "NULL"
      legend_shape <- shared_values$legend_shape %||% "NULL"
      text_size <- shared_values$predict_text_size %||% 12
      xlab <- shared_values$predict_xaxis %||% "Concentration"
      ylab <- shared_values$predict_yaxis %||% "Percent"
      title <- shared_values$predict_title %||% ""
      big_mark <- ifelse(shared_values$current_language == "French", " ", ",")
      trans <- shared_values$transformation %||% "log10"
      xbreaks <- shared_values$xbreaks %||% c()
      xbreaks_str <- paste0("c(", paste(xbreaks, collapse = ", "), ")")
      label_size <- shared_values$predict_label_size %||% 3
      shift_x <- shared_values$predict_shift_x %||% 1.05
      palette <- shared_values$predict_palette %||% "Set1"
      
      pred <- paste0("pred <- predict(dist, proportion = unique(c(1:99, ", threshold_vals$percent, ")/100))")
      plot <- paste0(
        "ssd_plot(data, pred, left = '", make.names(shared_values$selected_conc),
        "', label = ", code_label(),
        ", shape = ", code_shape(),
        ", color = ", code_colour(),
        ",  <br/>label_size = ", label_size,
        ", ylab = '", ylab,
        "', xlab = '", xlab,
        "', ci = FALSE, shift_x = ", shift_x,
        ", hc = ", code_hc(),
        ", <br/>big.mark = '", big_mark,
        "', trans = '", trans,
        "', xlimits = ", xlimits,
        ", xbreaks = ", xbreaks_str,
        ", text_size = ", text_size,
        ", theme_classic = TRUE",
        ") + <br/> ggtitle('", title,
        "') + <br/>scale_color_brewer(palette = '", palette, "', name = ", legend_colour, ") +<br/>
                         scale_shape(name = ", legend_shape, ")"
      )
      HTML(paste(pred, plot, sep = "<br/>"))
    })
    
    output$codePredCl <- renderUI({
      req(shared_values$bootstrap_samples)
      req(shared_values$fitted_dist)
      req(shared_values$threshold_values)
      
      threshold_vals <- shared_values$threshold_values
      thresh_type <- shared_values$thresh_type %||% "Concentration"
      
      form <- "ssd_hc"
      arg <- "proportion"
      thresh <- threshold_vals$percent / 100
      if (thresh_type != "Concentration") {
        form <- "ssd_hp"
        arg <- "conc"
        thresh <- threshold_vals$conc
      }
      
      nboot <- as.integer(gsub(",", "", shared_values$bootstrap_samples))
      
      conf <- paste0(
        paste0(form, "(dist, ", arg, " = "), thresh, ", ci = TRUE",
        ", nboot = ", nboot, "L, min_pboot = 0.8)"
      )
      conf2 <- paste0(
        paste0(form, "(dist, ", arg, " = "), thresh, ", ci = TRUE, average = FALSE",
        ", nboot = ", nboot, "L, min_pboot = 0.8)"
      )
      bind <- paste0("dplyr::bind_rows(", conf, ", ", conf2, ")")
      HTML(paste(bind, sep = "<br/>"))
    })
    
    output$codeSaveFit <- renderUI({
      req(shared_values$fitted_dist)
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
      req(shared_values$fitted_dist)
      req(shared_values$predictions)
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