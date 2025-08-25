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
                mod_data_ui("data_module")
              ),
# __ fit ------------------------------------------------------------------
              conditionalPanel(
                condition = "input.main_nav == 'fit'",
                mod_fit_ui("fit_module")
              ),

# __ predict --------------------------------------------------------------
              conditionalPanel(
                condition = "input.main_nav == 'predict'",
                mod_predict_ui("predict_module")
              )
            ),
# main content ------------------------------------------------------------
# __ data -----------------------------------------------------------------
            conditionalPanel(
              condition = "input.main_nav == 'data'",
              conditionalPanel(
                condition = "output.showDataResults",
                card(
                  card_header(span(`data-translate` = "ui_1preview", "Preview chosen dataset")),
                  card_body(DT::DTOutput("viewUpload"), min_height = "550px")
                ),
                card(class = "mt-3", card_body(span(`data-translate` = "ui_1note", "Note: the app is designed to handle one chemical at a time. Each species should not have more than one concentration value.")))
              )
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
                  ),
                  card(
                    full_screen = TRUE,
                    card_header(
                      class = "d-flex justify-content-between align-items-center",
                      span(`data-translate` = "ui_2table", "Goodness of Fit"),
                      actionButton("dlFitTable",
                                   label = tagList(bsicons::bs_icon("download"), span(`data-translate` = "ui_2download", "Download")),
                                   style = "padding:4px; font-size:80%"
                      )
                    ),
                    card_body(
                      div(id = "gofDiv",
                          DT::dataTableOutput("gofTable")
                      ),
                      # Hidden download button for programmatic triggering
                      downloadButton("fitDlTableHidden", "", style = "display: none;")
                    )
                  )
                ),
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
                      downloadButton("dlPredTable",
                                     label = tagList(bsicons::bs_icon("download"), span(`data-translate` = "ui_2download", "Download")),
                                     icon = NULL,
                                     style = "padding:4px; font-size:80%"
                      )
                    ),
                    card_body(
                      div(
                        class = "d-flex gap-4 align-items-start mb-3",
                        div(
                          selectInput("bootSamp",
                                      label = div(
                                        span(`data-translate` = "ui_3samples", "Bootstrap samples"),
                                        bslib::tooltip(
                                          bsicons::bs_icon("question-circle", style = "margin-left: 0.5rem; color: #6c757d; outline: none; border: none;"),
                                          span(`data-translate` = "ui_3bshint", "10,000 bootstrap samples recommended"),
                                          placement = "right"
                                        )
                                      ),
                                      choices = c("500" = "500", "1,000" = "1000", "5,000" = "5000", "10,000" = "10000"),
                                      selected = "1000",
                                      width = "190px")
                        ),
                        div(
                          actionButton("getCl", 
                                       label = span(`data-translate` = "ui_3clbutton", "Get CL"),
                                       class = "btn-primary",
                                       style = "white-space: nowrap;")
                        )
                      ),
                      # # Bootstrap samples and Get CL button at top
                      # div(
                      #   class = "d-flex justify-content-start align-items-end gap-3 mb-3",
                      #   div(
                      #     class = "flex-shrink-0",
                      #     
                      #   ),
                      #   div(
                      #     class = "flex-shrink-0",
                      #     
                      #   )
                      # ),
                      # Description text
                      div(
                        class = "mb-3",
                        htmlOutput("describeCl")
                      ),
                      # Results table
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
                card_body(mod_report_ui("report_module"))
              )
            ),

# __ r code ---------------------------------------------------------------
            conditionalPanel(
              condition = "input.main_nav == 'rcode'",
              card(
                card_header(uiOutput("ui_nav4")),
                card_body(mod_rcode_ui("rcode_module"))
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