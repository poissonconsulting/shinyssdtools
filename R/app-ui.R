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

# The app's page: a navbar with one tab per step (Data -> Fit -> Predict ->
# Export), each with a marker once it is done, then Help and the language menu.

# Inline-flex and vertically centred, so the brand sits on the navbar's centre
# line with the step tabs. The subtitle shows from the xl breakpoint, so the
# brand leaves the step tabs room on one line.
brand <- function() {
  div(
    class = "d-inline-flex align-items-center align-middle gap-3",
    ssd_art(),
    div(
      class = "lh-sm",
      div(class = "fs-5 fw-semibold", "ssdtools"),
      div(
        class = "small fw-normal ssd-brand-subtitle d-none d-xl-block",
        span(`data-translate` = "ui_navtitle", "Fit and Plot Species Sensitivity Distributions")
      )
    )
  )
}

# A step's tab: its name and, once it is done or while it runs in the
# background, a marker. The markers are switched in the browser by the output
# mark_<step> (app_server()), so they need no round trip to render.
step_title <- function(translate_key, default_text, step) {
  span(
    class = "d-inline-flex align-items-center gap-2",
    step_name(translate_key, default_text),
    span(class = "order-first d-inline-flex", step_marker_switch(step))
  )
}

# A step's name without its number, which its marker shows ("1. Data" ->
# "Data"); translation.js strips the number from translations too.
step_name <- function(translate_key, default_text) {
  span(`data-translate` = translate_key, `data-strip-number` = "", sub("^\\s*\\d+\\.\\s*", "", default_text))
}

# A step's marker in each state, switched in the browser by the output
# mark_<step> ("todo", "done" or "busy"): its number until it is done, then a
# tick, or a spinner while it runs in the background.
step_marker_switch <- function(step) {
  number <- match(step, c("data", "fit", "predict", "export"))
  marker <- function(state, condition, content) {
    # conditionalPanel() as a span, so the marker sits inline with the name.
    # Shiny shows a conditional element with display: contents, so the marker's
    # own box is an inner span.
    span(
      `data-display-if` = condition,
      `data-ns-prefix` = "",
      span(class = paste0("ssd-step-marker ssd-step-", state), content)
    )
  }
  tagList(
    marker(
      "todo",
      sprintf("['done', 'busy'].indexOf(output.mark_%s) < 0", step),
      span(`aria-hidden` = "true", number)
    ),
    marker(
      "done",
      sprintf("output.mark_%s === 'done'", step),
      tagList(lucide("check"), span(class = "visually-hidden", `data-translate` = "ui_step_done", "complete"))
    ),
    marker(
      "busy",
      sprintf("output.mark_%s === 'busy'", step),
      tagList(lucide("loader-2", "ssd-spin"), span(class = "visually-hidden", `data-translate` = "ui_step_busy", "running"))
    )
  )
}

# A tab's content as the page's main landmark. Only the open tab is shown, so
# only one main is visible at a time.
step_main <- function(...) tags$main(class = "py-4", ...)

language_menu <- function() {
  nav_menu(
    title = span(
      class = "d-inline-flex align-items-center gap-2",
      lucide("languages"),
      span(`data-translate` = "ui_navlang", "Language")
    ),
    align = "right",
    nav_item(actionLink(inputId = "english", label = "English", class = "dropdown-item")),
    nav_item(actionLink(inputId = "french", label = "Fran\u00e7ais", class = "dropdown-item"))
    # Spanish disabled
    # nav_item(actionLink(inputId = "spanish", label = "Espa\u00f1ol", class = "dropdown-item"))
  )
}

app_ui <- function() {
  page_navbar(
    title = brand(),
    id = "main_nav",
    theme = app_theme(),
    fluid = FALSE,
    fillable = FALSE,
    window_title = "ssdtools",
    lang = "en",
    navbar_options = navbar_options(collapsible = TRUE, underline = FALSE, theme = "dark"),
    header = tagList(
      shinyjs::useShinyjs(),
      rclipboard::rclipboardSetup(),
      # Spinners on outputs while they recalculate; the pulse would flash at
      # the top of the page on every input change.
      useBusyIndicators(pulse = FALSE),
      tags$head(
        # Versioned by the files' modification times, so a browser fetches
        # them again when they change rather than using a cached copy.
        tags$link(rel = "stylesheet", href = paste0("style.css?v=", asset_version("style.css"))),
        tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
        tags$script(src = paste0("translation.js?v=", asset_version("translation.js"))),
        tags$script(src = paste0("download.js?v=", asset_version("download.js")))
      )
    ),
    nav_spacer(),
    nav_panel(step_title("ui_nav1", "1. Data", "data"), value = "data", step_main(mod_data_ui("data_mod"))),
    nav_panel(step_title("ui_nav2", "2. Fit", "fit"), value = "fit", step_main(mod_fit_ui("fit_mod"))),
    nav_panel(step_title("ui_nav3", "3. Predict", "predict"), value = "predict", step_main(mod_predict_ui("predict_mod"))),
    nav_panel(
      step_title("ui_navexport", "4. Export", "export"),
      value = "export",
      step_main(mod_export_ui("export_mod", rcode = mod_rcode_ui("rcode_mod")))
    ),
    nav_panel(
      span(
        class = "d-inline-flex align-items-center gap-2",
        lucide("circle-help"),
        span(`data-translate` = "ui_navhelp", "Help")
      ),
      value = "help",
      step_main(help_ui())
    ),
    language_menu(),
    footer = div(
      class = "border-top text-center small text-body-secondary py-4 mt-4",
      sprintf(
        "shinyssdtools v%s \u00b7 ssdtools v%s",
        utils::packageVersion("shinyssdtools"),
        utils::packageVersion("ssdtools")
      )
    )
  )
}

# The Help tab: the user guide, methods and about pages, picked in its
# sidebar.
help_ui <- function() {
  page <- function(value, icon, translate_key, default_text, content) {
    nav_panel(
      title = span(
        class = "d-inline-flex align-items-center gap-2",
        lucide(icon),
        span(`data-translate` = translate_key, default_text)
      ),
      value = value,
      page_header(span(`data-translate` = translate_key, default_text)),
      content
    )
  }
  navset_pill_list(
    id = "help_page",
    well = FALSE,
    widths = c(3, 9),
    page("guide", "book-open", "ui_navguide", "User guide", card(card_body(uiOutput("ui_userguide")))),
    page("methods", "flask", "ui_navmethods", "Methods", uiOutput("ui_methods")),
    page("about", "info", "ui_navabout", "About", uiOutput("ui_about"))
  )
}

asset_version <- function(file) {
  path <- system.file("app", "www", file, package = "shinyssdtools")
  as.integer(file.mtime(path))
}
