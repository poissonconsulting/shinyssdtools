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
            class = "r-code-container",
            style = "
              background-color: #f8f9fa; 
              color: #212529; 
              padding: 1.5rem; 
              border-radius: 8px; 
              border: 1px solid #dee2e6;
              font-family: 'Fira Code', 'Consolas', 'Monaco', 'Courier New', monospace; 
              font-size: 14px; 
              line-height: 1.5;
              max-height: 70vh;
              overflow-y: auto;
            ",
            tags$style(HTML("
              .r-code-container pre {
                background: transparent !important;
                border: none !important;
                padding: 0 !important;
                margin: 0.5rem 0 !important;
                white-space: pre-wrap !important;
                word-wrap: break-word !important;
                font-family: inherit !important;
                font-size: inherit !important;
                color: inherit !important;
              }
            ")),
            uiOutput(ns("codeHead")),
            uiOutput(ns("codeData")),
            uiOutput(ns("codeFit")),
            uiOutput(ns("codeSaveFit")),
            uiOutput(ns("codePredPlot")),
            uiOutput(ns("codeSavePred")),
            uiOutput(ns("codePredCl"))
          )
        )
      )
    )
  )
}

# R Code Module Server
# Helper function to format R code with proper indentation
format_r_code <- function(code_lines) {
  # Join lines and format
  code_text <- paste(code_lines, collapse = "\n")
  
  # Basic indentation rules
  formatted_lines <- strsplit(code_text, "\n")[[1]]
  indent_level <- 0
  formatted_code <- c()
  
  for (line in formatted_lines) {
    trimmed <- trimws(line)
    if (nchar(trimmed) == 0) {
      formatted_code <- c(formatted_code, "")
      next
    }
    
    # Decrease indent for closing brackets
    if (grepl("^[\\)\\}]", trimmed)) {
      indent_level <- max(0, indent_level - 1)
    }
    
    # Add indentation
    indented_line <- paste0(strrep("  ", indent_level), trimmed)
    formatted_code <- c(formatted_code, indented_line)
    
    # Increase indent for opening brackets and function calls with (
    if (grepl("[\\(\\{]\\s*$", trimmed) || grepl("\\($", trimmed)) {
      indent_level <- indent_level + 1
    }
  }
  
  paste(formatted_code, collapse = "\n")
}

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
    
    # Individual code outputs with clean formatting
    output$codeHead <- renderUI({
      req(data_mod$has_data())
      data <- data_mod$data()
      if (is.null(data) || nrow(data) == 0) {
        return()
      }
      code_lines <- c(
        "install.packages('ssdtools')",
        "library(ssdtools)",
        "library(ggplot2)", 
        "library(dplyr)",
        "library(readr)"
      )
      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
    })

    output$codeData <- renderUI({
      req(data_mod$has_data())
      clean_data <- data_mod$clean_data()
      data_str <- utils::capture.output(dput(clean_data)) %>% glue::glue_collapse()
      code_lines <- c(
        paste0("data <- ", data_str),
        "colnames(data) <- make.names(colnames(data))"
      )
      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
    })

    output$codeFit <- renderUI({
      req(fit_mod$has_fit())
      ylab <- fit_mod$yaxis_label()
      xlab <- fit_mod$xaxis_label()
      text_size <- fit_mod$text_size()
      dists_str <- paste0("c(", paste0("'", fit_mod$dists(), "'", collapse = ", "), ")")
      
      code_lines <- c(
        paste0("dist <- ssd_fit_dists("),
        paste0("  data,"),
        paste0("  left = '", fit_mod$conc_column() %>% make.names(), "',"),
        paste0("  dists = ", dists_str, ","),
        paste0("  silent = TRUE,"),
        paste0("  reweight = FALSE,"),
        paste0("  rescale = ", fit_mod$rescale()),
        ")",
        "",
        paste0("ssd_plot_cdf("),
        paste0("  dist,"),
        paste0("  ylab = '", ylab, "',"),
        paste0("  xlab = '", xlab, "',"),
        paste0("  delta = Inf,"),
        paste0("  average = NA,"),
        paste0("  theme_classic = TRUE,"),
        paste0("  text_size = ", text_size),
        ")",
        "",
        "ssd_gof(dist) %>%",
        "  dplyr::mutate_if(is.numeric, ~ signif(., 3))"
      )
      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
    })

    output$codeSaveFit <- renderUI({
      req(fit_mod$has_fit())
      code_lines <- c(
        paste0("ggsave("),
        paste0("  'fit_dist_plot.png',"),
        paste0("  width = ", get_width2(), ","),
        paste0("  height = ", get_height2(), ","),
        paste0("  dpi = ", get_dpi2()),
        ")"
      )
      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
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
      trans <- ifelse(predict_mod$x_log(), "log10", "identity")
      xbreaks <- paste0("c(", paste(predict_mod$xbreaks(), collapse = ", "), ")")
      
      code_lines <- c(
        paste0("pred <- predict("),
        paste0("  dist,"),
        paste0("  proportion = unique(c(1:99, ", threshold_vals$percent, ") / 100)"),
        ")",
        "",
        paste0("ssd_plot("),
        paste0("  data,"),
        paste0("  pred,"),
        paste0("  left = '", make.names(fit_mod$conc_column()), "',"),
        paste0("  label = ", code_label(), ","),
        paste0("  shape = ", code_shape(), ","),
        paste0("  color = ", code_colour(), ","),
        paste0("  label_size = ", predict_mod$label_size(), ","),
        paste0("  ylab = '", ylab, "',"),
        paste0("  xlab = '", xlab, "',"),
        paste0("  ci = FALSE,"),
        paste0("  shift_x = ", predict_mod$adjust_label(), ","),
        paste0("  hc = ", code_hc(), ","),
        paste0("  big.mark = ',',"),
        paste0("  trans = '", trans, "',"),
        paste0("  xlimits = ", xlimits, ","),
        paste0("  xbreaks = ", xbreaks, ","),
        paste0("  text_size = ", text_size, ","),
        paste0("  theme_classic = TRUE"),
        ") +",
        paste0("  ggtitle('", title, "') +"),
        paste0("  scale_color_brewer(palette = '", predict_mod$palette(), "', name = ", legend.colour, ") +"),
        paste0("  scale_shape(name = ", legend.shape, ")")
      )
      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
    })

    output$codeSavePred <- renderUI({
      req(fit_mod$has_fit())
      req(predict_mod$has_predict())
      req(predict_mod$select_label())
      code_lines <- c(
        paste0("ggsave("),
        paste0("  'model_average_plot.png',"),
        paste0("  width = ", get_width(), ","),
        paste0("  height = ", get_height(), ","),
        paste0("  dpi = ", get_dpi()),
        ")"
      )
      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
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
      
      code_lines <- c(
        paste0("cl_average <- ", form, "("),
        paste0("  dist,"),
        paste0("  ", arg, " = ", thresh, ","),
        paste0("  ci = TRUE,"),
        paste0("  nboot = ", nboot_clean, "L,"),
        paste0("  min_pboot = 0.8"),
        ")",
        "",
        paste0("cl_individual <- ", form, "("),
        paste0("  dist,"),
        paste0("  ", arg, " = ", thresh, ","),
        paste0("  ci = TRUE,"),
        paste0("  average = FALSE,"),
        paste0("  nboot = ", nboot_clean, "L,"),
        paste0("  min_pboot = 0.8"),
        ")",
        "",
        "dplyr::bind_rows(cl_average, cl_individual)"
      )
      formatted_code <- format_r_code(code_lines)
      HTML(paste0("<pre>", formatted_code, "</pre>"))
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