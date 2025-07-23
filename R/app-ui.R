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
      ".shiny-output-error:before { visibility: hidden; }"
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
            bslib::navset_underline(
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
          bslib::layout_sidebar(
            padding = 0,
            gap = 0,
            sidebar = bslib::sidebar(
              width = 300,
              # Data Controls
              conditionalPanel(
                condition = "input.main_nav == 'data'",
                # uiOutput("ui_1choose"),
                uiOutput("ui_1data"),
                actionLink(
                  "infoDemo",
                  icon = icon("info-circle"),
                  label = NULL
                ),
                shinyjs::hidden(div(id = "infoDemoText", uiOutput("ui_1datahelp"))),
                br(),
                uiOutput("ui_1csv"),
                actionLink(
                  "infoUpload",
                  icon = icon("info-circle"),
                  label = NULL
                ),
                shinyjs::hidden(div(id = "infoUploadText", uiOutput("ui_1csvhelp"))),
                uiOutput("ui_1csvupload"),
                uiOutput("ui_1table1"),
                actionLink(
                  "infoHands",
                  icon = icon("info-circle"),
                  label = NULL
                ),
                shinyjs::hidden(div(id = "infoHandsText", uiOutput("ui_1tablehelp"))),
                rhandsontable::rHandsontableOutput("hot")
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
                  bslib::layout_column_wrap(
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
                  bslib::layout_column_wrap(width = 1 /
                                              2, uiOutput("ui_3size"), uiOutput("ui_3sizeLabel")),
                  uiOutput("ui_checkHc"),
                  bslib::layout_column_wrap(
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
                  bslib::layout_column_wrap(
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
              bslib::card(
                bslib::card_header(style = "background-color: #759dbe; color: white;", ),
                bslib::card_body(uiOutput("ui_1preview"), uiOutput("ui_viewupload"))
              ),
              bslib::card(class = "mt-3", bslib::card_body(uiOutput("ui_1note1")))
            ),
            # Fit Page Content
            conditionalPanel(
              condition = "input.main_nav == 'fit'",
              bslib::card(
                bslib::card_header(style = "background-color: #759dbe; color: white;", "Plot Fitted Distributions"),
                bslib::card_body(
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
                bslib::card(
                  class = "mt-3",
                  bslib::card_header(style = "background-color: #759dbe; color: white;", uiOutput("ui_2table")),
                  bslib::card_body(DT::dataTableOutput("gofTable"))
                )
              )
            ),
            # Predict Page Content
            conditionalPanel(
              condition = "input.main_nav == 'predict'",
              bslib::card(
                bslib::card_header(style = "background-color: #759dbe; color: white;", "Model Average Plot"),
                bslib::card_body(
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
                bslib::card(
                  class = "mt-3",
                  bslib::card_header(style = "background-color: #759dbe; color: white;", "Confidence Limits"),
                  bslib::card_body(
                    conditionalPanel(
                      condition = "output.clTable",
                      div(style = "margin-bottom: 1rem; display: inline-block;", uiOutput("ui_3dltable"))
                    ),
                    uiOutput("ui_3cl"),
                    actionLink("infoCl", icon = icon("info-circle"), label = NULL),
                    shinyjs::hidden(div(id = "clInfoText", uiOutput("ui_3help"))),
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
              bslib::card(
                bslib::card_header(style = "background-color: #759dbe; color: white;", "BCANZ Report Generation"),
                bslib::card_body(uiOutput("ui_report_download"))
              )
            ),
            # R Code Page Content
            conditionalPanel(
              condition = "input.main_nav == 'rcode'",
              bslib::card(
                bslib::card_header(style = "background-color: #759dbe; color: white;", uiOutput("ui_nav4")),
                bslib::card_body(
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