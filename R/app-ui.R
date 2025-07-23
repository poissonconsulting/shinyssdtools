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
      nav_panel(title = "Analyse", page_fillable(
        layout_sidebar(
          padding = 0,
          gap = 0,
          sidebar = sidebar(
            width = 175,
            bg = "#e5eff7",
            navset_underline(
              id = "main_nav",
              nav_panel(
                title = span(
                  `data-translate` = "ui_nav1",
                  bsicons::bs_icon("table"),
                  span(style = "margin-left: 0.5rem;", "1. Data")
                ),
                value = "data"
              ),
              nav_panel(
                title = span(
                  `data-translate` = "ui_nav2",
                  bsicons::bs_icon("graph-up"),
                  span(style = "margin-left: 0.5rem;", "2. Fit")
                ),
                value = "fit"
              ),
              nav_panel(
                title = span(
                  `data-translate` = "ui_nav3",
                  bsicons::bs_icon("calculator"),
                  span(style = "margin-left: 0.5rem;", "3. Predict")
                ),
                value = "predict"
              ),
              nav_panel(
                title = span(
                  `data-translate` = "ui_nav4",
                  bsicons::bs_icon("file-bar-graph"),
                  span(style = "margin-left: 0.5rem;", "4. BCANZ Report")
                ),
                value = "report"
              ),
              nav_panel(
                title = span(
                  `data-translate` = "ui_nav5",
                  bsicons::bs_icon("code-slash"),
                  span(style = "margin-left: 0.5rem;", "R Code")
                ),
                value = "rcode"
              )
            )
          ),
          layout_sidebar(
            padding = "1rem",
            gap = "1rem",
            sidebar = sidebar(
              width = 350,
              # Data Controls
              conditionalPanel(
                condition = "input.main_nav == 'data'",
                # Demo Data Section
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
              
              # Fit Controls
              conditionalPanel(
                condition = "input.main_nav == 'fit'",
                uiOutput("ui_conc"),
                uiOutput("ui_2select"),
                uiOutput("ui_2rescale"),
                uiOutput("ui_2xlab"),
                uiOutput("ui_2ylab"),
                uiOutput("ui_unit"),
                uiOutput("ui_2size"),
                uiOutput("ui_2png"),
                div(
                  id = "divFormatFit",
                  layout_column_wrap(
                    width = 1 / 3,
                    uiOutput("ui_2width"),
                    uiOutput("ui_2height"),
                    uiOutput("ui_2dpi")
                  )
                )
              ),
              # Predict Controls
              conditionalPanel(
                condition = "input.main_nav == 'predict'",
                h5("Prediction Settings"),
                style = "max-height: 70vh; overflow-y: auto;",
                uiOutput("ui_3est"),
                uiOutput("ui_3bshint"),
                uiOutput("ui_thresh_type"),
                uiOutput("ui_3thresh"),
                uiOutput("ui_3samples"),
                uiOutput("selectLabel"),
                uiOutput("selectColour"),
                uiOutput("selectShape"),
                uiOutput("ui_3plotopts"),
                div(
                  id = "divFormatPredict",
                  uiOutput("ui_3pal"),
                  uiOutput("ui_3xlab"),
                  uiOutput("ui_3ylab"),
                  uiOutput("ui_3title"),
                  uiOutput("uiLegendColour"),
                  uiOutput("uiLegendShape"),
                  layout_column_wrap(width = 1 /
                                              2, uiOutput("ui_3size"), uiOutput("ui_3sizeLabel")),
                  uiOutput("ui_checkHc"),
                  layout_column_wrap(
                    width = 1 / 3,
                    uiOutput("uiAdjustLabel"),
                    uiOutput("uiXmin"),
                    uiOutput("uiXmax")
                  ),
                  uiOutput("uiXlog"),
                  uiOutput("uiXbreaks")
                ),
                uiOutput("ui_3pngopts"),
                div(
                  id = "divPngFormatPredict",
                  layout_column_wrap(
                    width = 1 / 3,
                    uiOutput("ui_3width"),
                    uiOutput("ui_3height"),
                    uiOutput("ui_3dpi")
                  )
                )
              )
            ),
            # Data Page Content
            conditionalPanel(
              condition = "input.main_nav == 'data'",
              card(
                card_header(uiOutput("ui_1preview")),
                card_body(DT::DTOutput("viewUpload"), min_height = "550px")
              ),
              card(class = "mt-3", card_body(uiOutput("ui_1note1")))
            ),
            # Fit Page Content
            conditionalPanel(
              condition = "input.main_nav == 'fit'",
              div(
                class = "p-3",
                card(
                full_screen = TRUE,
                card_header("Plot Fitted Distributions"),
                card_body(
                  conditionalPanel(
                    condition = "output.distPlot1",
                    div(
                      style = "margin-bottom: 1rem; display: flex; gap: 0.5rem; flex-wrap: wrap;",
                      uiOutput("ui_2dlplot"),
                      uiOutput("ui_2dlrds"),
                      uiOutput("ui_2dltable")
                    )
                  ),
                  conditionalPanel(condition = "output.checkfit", htmlOutput("hintFi")),
                  conditionalPanel(condition = "output.distPlot1", uiOutput("ui_2plot")),
                  htmlOutput("fitFail"),
                  plotOutput("distPlot1")
                )
              ),
              conditionalPanel(
                condition = "output.gofTable",
                card(
                  class = "mt-3",
                  card_header(style = "background-color: #759dbe; color: white;", uiOutput("ui_2table")),
                  card_body(DT::dataTableOutput("gofTable"))
                )
              )
              )
            ),
            # Predict Page Content
            conditionalPanel(
              condition = "input.main_nav == 'predict'",
              card(
                card_header(style = "background-color: #759dbe; color: white;", "Model Average Plot"),
                card_body(
                  conditionalPanel(
                    condition = "output.modelAveragePlot",
                    div(style = "margin-bottom: 1rem; display: flex; gap: 0.5rem; flex-wrap: wrap;", uiOutput("ui_3dlplot"), uiOutput("ui_3dlrds"))
                  ),
                  conditionalPanel(condition = "output.checkpred", htmlOutput("hintPr")),
                  conditionalPanel(condition = "output.modelAveragePlot", uiOutput("ui_3model")),
                  plotOutput("modelAveragePlot"),
                  conditionalPanel(condition = "output.modelAveragePlot", htmlOutput("estHc"))
                )
              ),
              conditionalPanel(
                condition = "output.modelAveragePlot",
                card(
                  class = "mt-3",
                  card_header(style = "background-color: #759dbe; color: white;", "Confidence Limits"),
                  card_body(
                    conditionalPanel(
                      condition = "output.clTable",
                      div(style = "margin-bottom: 1rem; display: inline-block;", uiOutput("ui_3dltable"))
                    ),
                    uiOutput("ui_3cl"),
                    htmlOutput("describeCl"),
                    uiOutput("ui_3clbutton"),
                    conditionalPanel(condition = "output.modelAveragePlot", DT::dataTableOutput("clTable"))
                  )
                )
              )
            ),
            # BCANZ Report Page Content
            conditionalPanel(
              condition = "input.main_nav == 'report'",
              card(
                card_header(style = "background-color: #759dbe; color: white;", "BCANZ Report Generation"),
                card_body(uiOutput("ui_report_download"))
              )
            ),
            # R Code Page Content
            conditionalPanel(
              condition = "input.main_nav == 'rcode'",
              card(
                card_header(style = "background-color: #759dbe; color: white;", uiOutput("ui_nav4")),
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