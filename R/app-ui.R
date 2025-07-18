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
    # Dependencies need to be outside page_sidebar
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    
    # Hide shiny errors
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    bslib::page_sidebar(
      title = tagList(
        bslib::input_switch("language_switch", "Language: EN / FR", value = FALSE),
        uiOutput("ui_navtitle")
      ),
      
      sidebar = bslib::sidebar(
        width = 200,
        bslib::navset_pill(
          id = "main_nav",
          # widths = c(11, 1),
          bslib::nav_panel(
            title = HTML(paste(icon("table"), " 1. Data")),
            value = "data"
          ),
          bslib::nav_panel(
            title = HTML(paste(icon("chart-line"), " 2. Fit")),
            value = "fit"
          ),
          bslib::nav_panel(
            title = HTML(paste(icon("calculator"), " 3. Predict")),
            value = "predict"
          ),
          bslib::nav_panel(
            title = HTML(paste(icon("file-pdf"), " 4. BCANZ Report")),
            value = "report"
          ),
          bslib::nav_panel(
            title = HTML(paste(icon("code"), " R Code")),
            value = "rcode"
          ),
          bslib::nav_panel(
            title = "About",
            value = "about"
          ),
          bslib::nav_panel(
            title = "User Guide",
            value = "guide"
          )
        )
      ),
      
      # Data Page Content
      conditionalPanel(
        condition = "input.main_nav == 'data'",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Data Controls",
            width = 300,
            class = "border-0 bg-transparent",
            uiOutput("ui_1choose"),
            uiOutput("ui_1data"),
            actionLink("infoDemo", icon = icon("info-circle"), label = NULL),
            shinyjs::hidden(div(
              id = "infoDemoText",
              uiOutput("ui_1datahelp")
            )),
            
            br(),
            uiOutput("ui_1csv"),
            actionLink("infoUpload", icon = icon("info-circle"), label = NULL),
            shinyjs::hidden(div(
              id = "infoUploadText",
              uiOutput("ui_1csvhelp")
            )),
            uiOutput("ui_1csvupload"),
            
            uiOutput("ui_1table1"),
            actionLink("infoHands", icon = icon("info-circle"), label = NULL),
            shinyjs::hidden(div(
              id = "infoHandsText",
              uiOutput("ui_1tablehelp")
            )),
            rhandsontable::rHandsontableOutput("hot")
          ),
          
          # Data output area
          bslib::layout_columns(
            col_widths = 12,
            
            # Data Preview Card
            bslib::card(
              bslib::card_header(
                class = "bg-info text-white",
                "Data Preview"
              ),
              bslib::card_body(
                uiOutput("ui_1preview"),
                uiOutput("ui_viewupload")
              )
            ),
            
            # Note section
            bslib::card(
              class = "mt-3",
              bslib::card_body(
                uiOutput("ui_1note1")
              )
            )
          )
        )
      ),
      
      # Fit Page Content
      conditionalPanel(
        condition = "input.main_nav == 'fit'",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Model Configuration",
            width = 300,
            class = "border-0 bg-transparent",
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
                width = 1/3,
                uiOutput("ui_2width"),
                uiOutput("ui_2height"),
                uiOutput("ui_2dpi")
              )
            )
          ),
          
          # Fit output area
          bslib::layout_columns(
            col_widths = 12,
            
            # Plot Card
            bslib::card(
              bslib::card_header(
                class = "bg-primary text-white",
                "Species Sensitivity Distribution",
                div(
                  class = "float-end",
                  conditionalPanel(
                    condition = "output.distPlot1",
                    bslib::layout_column_wrap(
                      width = "auto",
                      uiOutput("ui_2dlplot"),
                      uiOutput("ui_2dlrds"),
                      uiOutput("ui_2dltable")
                    )
                  )
                )
              ),
              bslib::card_body(
                conditionalPanel(
                  condition = "output.checkfit",
                  htmlOutput("hintFi")
                ),
                conditionalPanel(
                  condition = "output.distPlot1",
                  uiOutput("ui_2plot")
                ),
                htmlOutput("fitFail"),
                plotOutput("distPlot1")
              )
            ),
            
            # Goodness of Fit Table Card
            conditionalPanel(
              condition = "output.gofTable",
              bslib::card(
                class = "mt-3",
                bslib::card_header(
                  class = "bg-warning text-dark",
                  uiOutput("ui_2table")
                ),
                bslib::card_body(
                  DT::dataTableOutput("gofTable")
                )
              )
            )
          )
        )
      ),
      
      # Predict Page Content
      conditionalPanel(
        condition = "input.main_nav == 'predict'",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            title = "Prediction Settings",
            width = 300,
            class = "border-0 bg-transparent",
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
              bslib::layout_column_wrap(
                width = 1/2,
                uiOutput("ui_3size"),
                uiOutput("ui_3sizeLabel")
              ),
              uiOutput("ui_checkHc"),
              bslib::layout_column_wrap(
                width = 1/3,
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
                width = 1/3,
                uiOutput("ui_3width"),
                uiOutput("ui_3height"),
                uiOutput("ui_3dpi")
              )
            )
          ),
          
          # Predict output area
          bslib::layout_columns(
            col_widths = 12,
            
            # Model Average Plot Card
            bslib::card(
              bslib::card_header(
                class = "bg-success text-white",
                "Model Average Plot",
                div(
                  class = "float-end",
                  conditionalPanel(
                    condition = "output.modelAveragePlot",
                    bslib::layout_column_wrap(
                      width = "auto",
                      uiOutput("ui_3dlplot"),
                      uiOutput("ui_3dlrds")
                    )
                  )
                )
              ),
              bslib::card_body(
                conditionalPanel(
                  condition = "output.checkpred",
                  htmlOutput("hintPr")
                ),
                conditionalPanel(
                  condition = "output.modelAveragePlot",
                  uiOutput("ui_3model")
                ),
                plotOutput("modelAveragePlot"),
                conditionalPanel(
                  condition = "output.modelAveragePlot",
                  htmlOutput("estHc")
                )
              )
            ),
            
            # Confidence Limits Card
            conditionalPanel(
              condition = "output.modelAveragePlot",
              bslib::card(
                class = "mt-3",
                bslib::card_header(
                  class = "bg-warning text-dark",
                  "Confidence Limits",
                  div(
                    class = "float-end",
                    conditionalPanel(
                      condition = "output.clTable",
                      uiOutput("ui_3dltable")
                    )
                  )
                ),
                bslib::card_body(
                  uiOutput("ui_3cl"),
                  actionLink("infoCl", icon = icon("info-circle"), label = NULL),
                  shinyjs::hidden(div(
                    id = "clInfoText",
                    uiOutput("ui_3help")
                  )),
                  htmlOutput("describeCl"),
                  uiOutput("ui_3clbutton"),
                  conditionalPanel(
                    condition = "output.modelAveragePlot",
                    DT::dataTableOutput("clTable")
                  )
                )
              )
            )
          )
        )
      ),
      
      # BCANZ Report Page Content
      conditionalPanel(
        condition = "input.main_nav == 'report'",
        bslib::card(
          bslib::card_header(
            class = "bg-danger text-white",
            "BCANZ Report Generation"
          ),
          bslib::card_body(
            uiOutput("ui_report_download")
          )
        )
      ),
      
      # R Code Page Content
      conditionalPanel(
        condition = "input.main_nav == 'rcode'",
        bslib::card(
          bslib::card_header(
            class = "bg-secondary text-white",
            uiOutput("ui_nav4")
          ),
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
      ),
      
      # About Page Content
      conditionalPanel(
        condition = "input.main_nav == 'about'",
        bslib::card(
          bslib::card_header(
            class = "bg-info text-white",
            "About shinyssdtools"
          ),
          bslib::card_body(
            uiOutput("ui_about")
          )
        )
      ),
      
      # User Guide Page Content
      conditionalPanel(
        condition = "input.main_nav == 'guide'",
        bslib::card(
          bslib::card_header(
            class = "bg-primary text-white",
            "User Guide"
          ),
          bslib::card_body(
            uiOutput("ui_userguide")
          )
        )
      )
    )
  )
}