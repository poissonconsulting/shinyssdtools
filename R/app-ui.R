# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

app_ui <- function() {
  tagList(
    # Dependencies
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    
    # Include custom JavaScript for translations
    tags$script(src = "translation.js"),
    
    # Hide shiny errors
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }",
      "/* Remove focus outline from help icons */",
      ".bi-question-circle:focus { outline: none !important; border: none !important; box-shadow: none !important; }",
      ".bi-question-circle { cursor: pointer; }"
    ),
    
    page_navbar(
      title = "shinyssdtools",
      navbar_options = navbar_options(bg = "#759dbe", underline = TRUE),
      nav_panel(title = span(`data-translate` = "ui_navanalyse", "Analyse"), page_fillable(
        layout_sidebar(
          padding = 0,
          gap = 0,

# nav ---------------------------------------------------------------------
          sidebar = sidebar(
            width = 175,
            bg = "#e5eff7",
            navset_underline(
              id = "main_nav",
              nav_panel(
                title = span(
                  bsicons::bs_icon("table"),
                  span(`data-translate` = "ui_nav1", style = "margin-left: 0.5rem;", "1. Data")
                ),
                value = "data"
              ),
              nav_panel(
                title = span(
                  bsicons::bs_icon("graph-up"),
                  span(`data-translate` = "ui_nav2", style = "margin-left: 0.5rem;", "2. Fit")
                ),
                value = "fit"
              ),
              nav_panel(
                title = span(
                  bsicons::bs_icon("calculator"),
                  span(`data-translate` = "ui_nav3", style = "margin-left: 0.5rem;", "3. Predict")
                ),
                value = "predict"
              ),
              nav_panel(
                title = span(
                  bsicons::bs_icon("file-bar-graph"),
                  span(`data-translate` = "ui_nav4", style = "margin-left: 0.5rem;", "4. Report")
                ),
                value = "report"
              ),
              nav_panel(
                title = span(
                  bsicons::bs_icon("code-slash"),
                  span(`data-translate` = "ui_nav5", style = "margin-left: 0.5rem;", "R Code")
                ),
                value = "rcode"
              )
            )
          ),

# controls ----------------------------------------------------------------
          layout_sidebar(
            padding = "1rem",
            gap = "1rem",
            sidebar = sidebar(
              width = 350,

# __data ------------------------------------------------------------------
              conditionalPanel(
                condition = "input.main_nav == 'data'",
                # Demo Data Section
                span(`data-translate` = "ui_1choose", "Choose one of the following options:"),
                p(
                  bslib::popover(
                    bsicons::bs_icon("question-circle", style = "margin-right: 0.5rem; color: #6c757d; outline: none; border: none;"),
                    div(
                      span(`data-translate` = "ui_1datahelp", "This can be used to demo the app or view a dataset that 'works'."),
                      br(), br(),
                      "Citation:",
                      tags$a("Canadian Council of Ministers of the Environment. 2009. Canadian water quality guidelines for the protection of aquatic life: Boron. In: Canadian environmental quality guidelines, 2009, Canadian Council of Ministers of the Environment, Winnipeg.", href = "http://ceqg-rcqe.ccme.ca/download/en/324/")
                    ),
                    placement = "right"
                  ),
                  span(
                    span(`data-translate` = "ui_1data", "1. Use "),
                    actionLink("demoData", span(`data-translate` = "ui_1data2", "boron dataset"), icon = icon("table"))
                  )
                ),
                # CSV Upload Section  
                fileInput("uploadData",
                  buttonLabel = span(tagList(icon("upload"), "csv")),
                  label = span(
                    bslib::popover(
                      bsicons::bs_icon("question-circle", style = "margin-right: 0.5rem; color: #6c757d; outline: none; border: none;"),
                      span(`data-translate` = "ui_1csvhelp", "Upload your own CSV file with concentration and species data."),
                      placement = "right"
                    ),
                    span(`data-translate` = "ui_1csv", "2. Upload CSV file")
                  ),
                  placeholder = "...",
                  accept = c(".csv")
                ),
                # Data Table Section
                bslib::accordion(
                  open = FALSE,
                  bslib::accordion_panel(
                    title = span(
                      bslib::popover(
                        bsicons::bs_icon("question-circle", style = "margin-right: 0.5rem; color: #6c757d; outline: none; border: none;"),
                        span(`data-translate` = "ui_1tablehelp", "Manually enter concentration and species data in the table."),
                        placement = "right"
                      ),
                      span(`data-translate` = "ui_1table", "3. Fill out table below:")
                    ),
                    value = "data_table",
                    rhandsontable::rHandsontableOutput("hot")
                  )
                )
              ),
# __ fit ------------------------------------------------------------------
              conditionalPanel(
                condition = "input.main_nav == 'fit'",
                uiOutput("ui_conc"),
                selectizeInput("selectDist",
                               label = span(`data-translate` = "ui_2dist", "Distributions"),
                               multiple = TRUE,
                               choices = c(default.dists, extra.dists),
                               selected = default.dists,
                               options = list(
                                 "plugins" = list("remove_button"),
                                 "create" = TRUE,
                                 "persist" = FALSE
                               )
                ),
                uiOutput("ui_unit"),
                checkboxInput("rescale",
                  label = span(`data-translate` = "ui_2rescale", "Rescale"),
                  value = FALSE
                ),
                textInput("xaxis2",
                  label = span(`data-translate` = "ui_3xlab", "X-axis label"),
                  value = "Concentration"
                ),
                textInput("yaxis2",
                  label = span(`data-translate` = "ui_3ylab", "Y-axis label"),
                  value = "Percent"
                ),
                numericInput("size2",
                  label = span(`data-translate` = "ui_size", "Text size"),
                  value = 12, min = 1, max = 100
                )
              ),

# __ predict --------------------------------------------------------------
              conditionalPanel(
                condition = "input.main_nav == 'predict'",
                style = "max-height: 70vh; overflow-y: auto;",
                span(`data-translate` = "ui_3est", "Estimate hazard concentration"),
                p(class = "text-muted small", span(`data-translate` = "ui_3bshint", "10,000 bootstrap samples recommended")),
                uiOutput("ui_thresh_type"),
                uiOutput("ui_3thresh"),
                uiOutput("ui_3samples"),
                uiOutput("selectLabel"),
                uiOutput("selectColour"),
                uiOutput("selectShape"),
                accordion(open = FALSE,
                  accordion_panel(title = span(`data-translate` = "ui_3plotopts", "Plot formatting options"),
                                  value = "plot-formatting",
                                  uiOutput("ui_3pal"),
                                  textInput("xaxis", 
                                            value = "Concentration", 
                                            label = span(`data-translate` = "ui_3xlab", "X-axis label")),
                                  textInput("yaxis", 
                                            value = "Percent", 
                                            label = span(`data-translate` = "ui_3ylab", "Y-axis label")),
                                  textInput("title", 
                                            value = "", 
                                            label = span(`data-translate` = "ui_3title", "Title")),
                                  uiOutput("uiLegendColour"),
                                  uiOutput("uiLegendShape"),
                                  layout_column_wrap(width = 1 / 2, 
                                                     numericInput("size3", 
                                                                  label = span(`data-translate` = "ui_size", "Text size"), 
                                                                  value = 12, min = 1, max = 100),
                                                     numericInput("sizeLabel3", 
                                                                  label = span(`data-translate` = "ui_sizeLabel", "Label size"), 
                                                                  value = 3, min = 1, max = 10)
                                  ),
                                  checkboxInput("checkHc", 
                                                label = span(`data-translate` = "ui_checkHc", "Show hazard concentration"), 
                                                value = TRUE),
                                  layout_column_wrap(
                                    width = 1 / 3,
                                    numericInput("adjustLabel",
                                                 value = 1.05, 
                                                 label = span(`data-translate` = "ui_adjustLabel", "Adjust label"), 
                                                 min = 0, max = 10, step = 0.1),
                                    numericInput("xMin", 
                                                 label = span(`data-translate` = "ui_xmin", "X min"), 
                                                 min = 1, value = NULL),
                                    numericInput("xMax", 
                                                 label = span(`data-translate` = "ui_xmax", "X max"), 
                                                 min = 1, value = NULL)
                                  ),
                                  checkboxInput("xlog", 
                                                label = span(`data-translate` = "ui_xlog", "Log scale"), 
                                                value = TRUE),
                                  uiOutput("uiXbreaks")
                  )
                ))),

# main content ------------------------------------------------------------
# __ data -----------------------------------------------------------------
            conditionalPanel(
              condition = "input.main_nav == 'data'",
              card(
                card_header(span(`data-translate` = "ui_1preview", "Preview chosen dataset")),
                card_body(DT::DTOutput("viewUpload"), min_height = "550px")
              ),
              card(class = "mt-3", card_body(span(`data-translate` = "ui_1note", "Note: the app is designed to handle one chemical at a time. Each species should not have more than one concentration value.")))
            ),

# __ fit ------------------------------------------------------------------
            conditionalPanel(
              condition = "input.main_nav == 'fit'",
              div(
                class = "p-3",
                conditionalPanel(condition = "output.checkfit", htmlOutput("hintFi")),
                conditionalPanel(condition = "output.showFitResults", uiOutput("ui_2plot")),
                conditionalPanel(
                  condition = "output.showFitResults",
                  card(
                    full_screen = TRUE,
                    card_header(
                      class = "d-flex justify-content-between align-items-center",
                      span(`data-translate` = "ui_2plot", "Plot Fitted Distributions"),
                      ui_download_popover()
                    ),
                    card_body(
                      htmlOutput("fitFail"),
                      plotOutput("distPlot1")
                    )
                  )
                ),
                conditionalPanel(
                  condition = "output.showFitResults",
                  card(
                    full_screen = TRUE,
                    card_header(
                      class = "d-flex justify-content-between align-items-center",
                      span(`data-translate` = "ui_2table", "Goodness of Fit"),
                      downloadButton("dlFitTable",
                                     label = tagList(bsicons::bs_icon("download"), span(`data-translate` = "ui_2download", "Download")),
                                     icon = NULL,
                                     style = "padding:4px; font-size:80%"
                      )
                    ),
                    card_body(
                      DT::dataTableOutput("gofTable")
                    )
                  )
                )
              )
            ),

# __ predict --------------------------------------------------------------
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
                      ui_predict_download_popover()
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
                      downloadButton("dlPredTable",
                                     label = tagList(bsicons::bs_icon("download"), span(`data-translate` = "ui_2download", "Download")),
                                     icon = NULL
                      )
                    ),
                    card_body(
                      div(
                        class = "d-flex justify-content-between align-items-start mb-3",
                        div(
                          class = "flex-grow-1 me-3",
                          htmlOutput("describeCl")
                        ),
                        div(
                          uiOutput("ui_3clbutton")
                        )
                      ),
                      conditionalPanel(condition = "output.showPredictResults", DT::dataTableOutput("clTable"))
                    )
                  )
                )
              )
            ),

# __ report ---------------------------------------------------------------
            conditionalPanel(
              condition = "input.main_nav == 'report'",
              card(
                card_header("BCANZ Report Generation"),
                card_body(uiOutput("ui_report_download"))
              )
            ),

# __ r code ---------------------------------------------------------------
            conditionalPanel(
              condition = "input.main_nav == 'rcode'",
              card(
                card_header(uiOutput("ui_nav4")),
                card_body(
                  uiOutput("ui_4help"),
                  div(
                    id = "codes",
                    style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; font-family: 'Courier New', monospace; font-size: 12px;",
                    uiOutput("codeHead"),
                    br(),
                    uiOutput("codeData"),
                    br(),
                    uiOutput("codeFit"),
                    br(),
                    uiOutput("codeSaveFit"),
                    br(),
                    uiOutput("codePredPlot"),
                    br(),
                    uiOutput("codeSavePred"),
                    br(),
                    uiOutput("codePredCl")
                  )
                )
              )
            )
          )
        )
      )),
      nav_panel(title = span(`data-translate` = "ui_navabout", "About"), card(card_body(
        uiOutput("ui_about")
      ))),
      nav_panel(title = span(`data-translate` = "ui_navguide", "User Guide"), card(
        uiOutput("ui_userguide")
      )),
      nav_spacer(),
      nav_menu(
        title = "Language",
        nav_item(actionLink(inputId = "english", label = "English")),
        nav_item(actionLink(inputId = "french", label = "Français"))
      )
    )
  )
}