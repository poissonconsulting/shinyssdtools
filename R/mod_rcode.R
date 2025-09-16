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
mod_rcode_server <- function(id, translations, data_mod, fit_mod, predict_mod) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper functions for code generation
    code_label <- reactive({
      label_val <- predict_mod$select_label()
      if (is.null(label_val) || label_val == "-none-") {
        return("NULL")
      }
      paste0("'", label_val %>% make.names(), "'")
    })
    
    code_colour <- reactive({
      colour_val <- predict_mod$select_colour()
      if (is.null(colour_val) || colour_val == "-none-") {
        return("NULL")
      }
      paste0("'", colour_val %>% make.names(), "'")
    })
    
    code_shape <- reactive({
      shape_val <- predict_mod$select_shape()
      if (is.null(shape_val) || shape_val == "-none-") {
        return("NULL")
      }
      paste0("'", shape_val %>% make.names(), "'")
    })
    
    code_hc <- reactive({
      threshold_vals <- predict_mod$threshold_values()
      show_hc <- predict_mod$check_hc()
      if (is.null(show_hc) || !show_hc || is.null(threshold_vals)) {
        return("NULL")
      }
      threshold_vals$percent / 100
    })
    
    # Get plot dimensions - using default values since download dimensions not in UI
    get_width <- reactive({
      6  # Default width for predict plots
    })
    
    get_width2 <- reactive({
      6  # Default width for fit plots
    })
    
    get_height <- reactive({
      4  # Default height for predict plots
    })
    
    get_height2 <- reactive({
      4  # Default height for fit plots
    })
    
    get_dpi <- reactive({
      300  # Default DPI for predict plots
    })
    
    get_dpi2 <- reactive({
      300  # Default DPI for fit plots
    })
    
    # Code section outputs
    output$ui_4help <- renderUI({
      trans_obj <- translations()
      HTML(tr("ui_4help", trans_obj))
    })
    
    # render code 
    output$codeHead <- renderUI({
      req(data_mod$has_data())
      data <- data_mod$data()
      if (is.null(data) || nrow(data) == 0) {
        return()
      }
      l1 <- "install.packages('ssdtools')"
      l2 <- "library(ssdtools)"
      l3 <- "library(ggplot2)"
      l4 <- "library(dplyr)"
      l5 <- "library(readr)"  # Always include readr for consistency
      HTML(paste(l1, l2, l3, l4, l5, sep = "<br/>"))
    })
    
    output$codeData <- renderUI({
      req(data_mod$has_data())
      # For simplicity, always use dput format since we can't determine upload source
      clean_data <- data_mod$clean_data()
      hot <- paste0("data <- ", utils::capture.output(dput(clean_data)) %>% glue::glue_collapse())
      name <- "colnames(data) <- make.names(colnames(data))"
      HTML(paste(hot, name, sep = "<br/>"))
    })
    
    output$codeFit <- renderUI({
      req(fit_mod$has_fit())
      ylab <- fit_mod$yaxis_label()
      xlab <- fit_mod$xaxis_label()
      text_size <- fit_mod$text_size()
      fit <- paste0(
        "dist <- ssd_fit_dists(data, left = '",
        fit_mod$conc_column() %>% make.names(),
        "', dists = c(",
        paste0("'", fit_mod$dists(), "'", collapse = ", "), ")",
        ", silent = TRUE, reweight = FALSE",
        ", rescale = ", fit_mod$rescale(), ")"
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
      req(fit_mod$has_fit())
      req(predict_mod$has_predict())
      req(predict_mod$select_label())
      
      threshold_vals <- predict_mod$threshold_values()
      xmax <- predict_mod$x_max()
      xmin <- predict_mod$x_min()
      xlimits <- ifelse(is.na(xmin) & is.na(xmax), "NULL", paste0("c(", xmin, ", ", xmax, ")"))
      legend.colour <- ifelse(is.null(predict_mod$legend_colour()) || predict_mod$legend_colour() == "-none-", "NULL", paste0("'", predict_mod$legend_colour(), "'"))
      legend.shape <- ifelse(is.null(predict_mod$legend_shape()) || predict_mod$legend_shape() == "-none-", "NULL", paste0("'", predict_mod$legend_shape(), "'"))
      text_size <- predict_mod$text_size()
      xlab <- predict_mod$xaxis_label()
      ylab <- predict_mod$yaxis_label()
      title <- predict_mod$title()
      big.mark <- ","  # Default to English format
      trans <- ifelse(predict_mod$x_log(), "log10", "identity")
      xbreaks <- predict_mod$xbreaks()
      xbreaks <- paste0("c(", paste(xbreaks, collapse = ", "), ")")
      pred <- paste0("pred <- predict(dist, proportion = unique(c(1:99, ", threshold_vals$percent, ")/100))")
      plot <- paste0(
        "ssd_plot(data, pred, left = '", make.names(fit_mod$conc_column()),
        "', label = ", code_label(),
        ", shape = ", code_shape(),
        ", color = ", code_colour(),
        ",  <br/>label_size = ", predict_mod$label_size(),
        ", ylab = '", ylab,
        "', xlab = '", xlab,
        "', ci = FALSE, shift_x = ", predict_mod$adjust_label(),
        ", hc = ", code_hc(),
        ", <br/>big.mark = '", big.mark,
        "', trans = '", trans,
        "', xlimits = ", xlimits,
        ", xbreaks = ", xbreaks,
        ", text_size = ", text_size,
        ", theme_classic = TRUE",
        ") + <br/> ggtitle('", title,
        "') + <br/>scale_color_brewer(palette = '", predict_mod$palette(), "', name = ", legend.colour, ") +<br/>
                     scale_shape(name = ", legend.shape, ")"
      )
      HTML(paste(pred, plot, sep = "<br/>"))
    })
    
    output$codePredCl <- renderUI({
      req(predict_mod$has_cl())
      req(fit_mod$has_fit())
      req(predict_mod$has_predict())
      
      threshold_vals <- predict_mod$threshold_values()
      form <- "ssd_hc"
      arg <- "proportion"
      thresh <- threshold_vals$percent / 100
      if (predict_mod$threshold_type() != "Concentration") {
        form <- "ssd_hp"
        arg <- "conc"
        thresh <- threshold_vals$conc
      }
      
      nboot_clean <- predict_mod$nboot() %>% gsub(",", "", .) %>% gsub("\\s", "", .) %>% as.integer()
      
      conf <- paste0(
        paste0(form, "(dist, ", arg, " = "), thresh, ", ci = TRUE",
        ", nboot = ", nboot_clean, "L, min_pboot = 0.8)"
      )
      conf2 <- paste0(
        paste0(form, "(dist, ", arg, " = "), thresh, ", ci = TRUE, average = FALSE",
        ", nboot = ", nboot_clean, "L, min_pboot = 0.8)"
      )
      bind <- paste0("dplyr::bind_rows(", conf, ", ", conf2, ")")
      HTML(paste(bind, sep = "<br/>"))
    })
    
    output$codeSaveFit <- renderUI({
      req(fit_mod$has_fit())
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
      req(fit_mod$has_fit())
      req(predict_mod$has_predict())
      req(predict_mod$select_label())
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
          fit_mod$has_fit() && data_mod$has_data()
        })
      )
    )
  })
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x