# Copyright 2015-2025 Province of British Columbia
# Copyright 2021 Environment and Climate Change Canada
# Copyright 2023-2025 Australian Government Department of Climate Change,
# Energy, the Environment and Water
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

app_ui <- function() {
  tagList(
    # Dependencies
    shinyjs::useShinyjs(),
    # Spinners on outputs while they recalculate; the pulse would flash at
    # the top of the page on every input change.
    useBusyIndicators(pulse = FALSE),
    rclipboard::rclipboardSetup(),

    # Versioned by the file's modification time, so a browser fetches the
    # stylesheet again when it changes rather than using a cached copy.
    tags$head(tags$link(
      rel = "stylesheet",
      href = paste0("style.css?v=", styles_version())
    )),

    # Include custom JavaScript for translations
    tags$script(src = "translation.js"),

    # Include custom JavaScript for popover dismiss behavior
    tags$script(src = "popover-dismiss.js"),

    # Add custom CSS handler for notifications
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('addCustomCSS', function(message) {
        var style = document.createElement('style');
        style.type = 'text/css';
        style.innerHTML = message.css;
        document.getElementsByTagName('head')[0].appendChild(style);
      });
    "
    )),

    tags$style(type = "text/css", ".initially-hidden { display: none; }"),

    tags$script(HTML(
      "
      $(document).on('shiny:connected', function() {
        $('.initially-hidden').removeClass('initially-hidden');
      });
    "
    )),

    page_navbar(
      title = "shinyssdtools",
      theme = app_theme(),
      lang = "en",
      navbar_options = navbar_options(bg = color_secondary, underline = TRUE),
      nav_panel(
        title = span(`data-translate` = "ui_navanalyse", "Analyse"),
        page_fillable(
          layout_sidebar(
            padding = 0,
            gap = 0,

            # nav ---------------------------------------------------------------------
            sidebar = sidebar(
              width = 180,
              bg = color_sidebar,
              navset_underline(
                id = "main_nav",
                nav_panel(
                  title = step_title("table", "ui_nav1", "1. Data", "data"),
                  value = "data"
                ),
                nav_panel(
                  title = step_title("graph-up", "ui_nav2", "2. Fit", "fit"),
                  value = "fit"
                ),
                nav_panel(
                  title = step_title("calculator", "ui_nav3", "3. Predict", "predict"),
                  value = "predict"
                ),
                nav_panel(
                  title = step_title("file-bar-graph", "ui_nav4", "4. Report", "report"),
                  value = "report"
                ),
                nav_panel(
                  title = step_title("code-slash", "ui_nav5", "R Code", "rcode"),
                  value = "rcode"
                )
              )
            ),
            div(
              class = "initially-hidden",
              conditionalPanel(
                condition = "input.main_nav == 'data'",
                mod_data_ui("data_mod")
              )
            ),
            div(
              class = "initially-hidden",
              conditionalPanel(
                condition = "input.main_nav == 'fit'",
                mod_fit_ui("fit_mod")
              )
            ),
            div(
              class = "initially-hidden",
              conditionalPanel(
                condition = "input.main_nav == 'predict'",
                mod_predict_ui("predict_mod")
              )
            ),
            div(
              class = "initially-hidden",
              conditionalPanel(
                condition = "input.main_nav == 'report'",
                mod_report_ui("report_mod")
              )
            ),
            div(
              class = "initially-hidden",
              conditionalPanel(
                condition = "input.main_nav == 'rcode'",
                mod_rcode_ui("rcode_mod")
              )
            )
          )
        )
      ),
      nav_panel(
        title = span(`data-translate` = "ui_navabout", "About"),
        card(class = card_shadow, card_body(uiOutput("ui_about")))
      ),
      nav_panel(
        title = span(`data-translate` = "ui_navguide", "User Guide"),
        card(class = card_shadow, uiOutput("ui_userguide"))
      ),
      nav_spacer(),
      nav_menu(
        title = span(`data-translate` = "ui_navlang", "Language"),
        align = "right",
        nav_item(
          actionLink(inputId = "english", label = "English")
        ),
        nav_item(
          actionLink(inputId = "french", label = "Fran\u00e7ais")
        )
        # Spanish disabled
        # nav_item(
        #   actionLink(inputId = "spanish", label = "Espa\u00f1ol")
        # )
      )
    )
  )
}

# A step in the step navigation: its icon and name, and a marker that shows
# once the step is done (a tick) or while it runs in the background (a
# spinner). The markers are switched in the browser by the output
# mark_<step>, set in app_server().
step_title <- function(icon, translate_key, default_text, step) {
  span(
    class = "d-inline-flex align-items-center gap-2 w-100",
    bsicons::bs_icon(icon, a11y = "deco"),
    span(`data-translate` = translate_key, default_text),
    if (step != "rcode") step_marker_switch(step)
  )
}

step_marker_switch <- function(step) {
  marker <- function(state, icon, translate_key, default_text) {
    # conditionalPanel() as a span, so the marker sits inline with the name.
    span(
      `data-display-if` = sprintf("output.mark_%s === '%s'", step, state),
      `data-ns-prefix` = "",
      icon,
      span(class = "visually-hidden", `data-translate` = translate_key, default_text)
    )
  }
  span(
    class = "ms-auto d-inline-flex",
    marker(
      "done",
      bsicons::bs_icon("check-circle-fill", class = "text-success", a11y = "deco"),
      "ui_step_done",
      "complete"
    ),
    marker("busy", busy_icon(), "ui_step_busy", "running")
  )
}

styles_version <- function() {
  path <- system.file("app", "www", "style.css", package = "shinyssdtools")
  as.integer(file.mtime(path))
}
