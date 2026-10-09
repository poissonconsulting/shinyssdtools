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

#' Create static label with dynamic input pattern
#' @param ns_id Character string namespaced input ID for label's 'for' attribute
#' @param translate_key Character string translation key for data-translate attribute
#' @param default_text Character string default text to display in label
#' @param ui_output_id Character string ID for the uiOutput element
#' @return tagList with label and uiOutput elements
#' @keywords internal
static_label_input <- function(
  ns_id,
  translate_key,
  default_text,
  ui_output_id
) {
  tagList(
    tags$label(
      `for` = ns_id,
      class = "control-label",
      span(`data-translate` = translate_key, default_text)
    ),
    uiOutput(ui_output_id)
  )
}

#' Build GOF-style header tooltips named vector
#' @param trans Translations reactive value
#' @param lang Character string language code ("english", "french", "spanish")
#' @return Named character vector mapping column names to tooltip descriptions
#' @keywords internal
gof_header_tooltips <- function(trans, lang = "english") {
  # Descriptions sourced from inst/extdata/about-{lang}.md
  tooltips <- switch(lang,
    "french" = c(
      dist = "Distribution",
      npars = "Nombre de param\u00e8tres",
      nobs = "Nombre d'observations",
      log_lik = "Log-vraisemblance",
      aic = "Crit\u00e8re d'information Akaike",
      aicc = "Crit\u00e8re d'information Akaike corrig\u00e9 pour la taille de l'\u00e9chantillon",
      delta = "Diff\u00e9rence entre AICc",
      wt = "Pond\u00e9ration des crit\u00e8res d'information AICc",
      weight = "Pond\u00e9ration des crit\u00e8res d'information AICc",
      bic = "Crit\u00e8re d'information Bay\u00e9sien",
      ad = "Statistique d'Anderson-Darling",
      ks = "Statistique de Kolmogorov-Smirnov",
      cvm = "Statistique de Cramer-von Mises",
      est = "Concentration ou pourcentage estim\u00e9",
      se = "Erreur type",
      lcl = "Limite de confiance inf\u00e9rieure",
      ucl = "Limite de confiance sup\u00e9rieure",
      nboot = "Nombre d'\u00e9chantillons bootstrap",
      pboot = "Proportion de bootstraps r\u00e9ussis",
      samples = "Nombre d'\u00e9chantillons",
      proportion = "Proportion d'esp\u00e8ces affect\u00e9es",
      percent = "Pourcentage d'esp\u00e8ces affect\u00e9es"
    ),
    # Default: English
    c(
      dist = "Distribution",
      npars = "Number of parameters",
      nobs = "Number of observations",
      log_lik = "Log-likelihood",
      aic = "Akaike's Information Criterion",
      aicc = "Akaike's Information Criterion corrected for sample size",
      delta = "AICc difference",
      wt = "AICc based Akaike weight",
      weight = "AICc based Akaike weight",
      bic = "Bayesian Information Criterion",
      ad = "Anderson-Darling statistic",
      ks = "Kolmogorov-Smirnov statistic",
      cvm = "Cramer-von Mises statistic",
      est = "Estimated concentration or percent",
      se = "Standard error",
      lcl = "Lower confidence limit",
      ucl = "Upper confidence limit",
      nboot = "Number of bootstrap samples",
      pboot = "Proportion of successful bootstraps",
      samples = "Number of samples",
      proportion = "Proportion of species affected",
      percent = "Percent of species affected"
    )
  )
  wt_translated <- tr("ui_2weight", trans)
  if (!wt_translated %in% names(tooltips)) {
    tooltips[[wt_translated]] <- tooltips[["wt"]]
  }
  tooltips
}

#' Language strings for reactable tables
#' @param lang Character string language: "english", "french" or "spanish".
#' @return A [reactable::reactableLang()] object.
#' @keywords internal
table_lang <- function(lang = "english") {
  switch(
    lang,
    "french" = reactable::reactableLang(
      searchPlaceholder = "Rechercher",
      noData = "Aucune donn\u00e9e",
      pageInfo = "{rowStart} \u00e0 {rowEnd} sur {rows} lignes",
      pagePrevious = "\u2039",
      pageNext = "\u203a",
      pagePreviousLabel = "Page pr\u00e9c\u00e9dente",
      pageNextLabel = "Page suivante"
    ),
    "spanish" = reactable::reactableLang(
      searchPlaceholder = "Buscar",
      noData = "No hay datos",
      pageInfo = "{rowStart} a {rowEnd} de {rows} filas",
      pagePrevious = "\u2039",
      pageNext = "\u203a",
      pagePreviousLabel = "P\u00e1gina anterior",
      pageNextLabel = "P\u00e1gina siguiente"
    ),
    reactable::reactableLang(
      searchPlaceholder = "Search",
      pageInfo = "{rowStart} to {rowEnd} of {rows} rows",
      pagePrevious = "\u2039",
      pageNext = "\u203a",
      pagePreviousLabel = "Previous page",
      pageNextLabel = "Next page"
    )
  )
}

#' Create a table in the app's style
#'
#' A reactable with the app's theme. Column names in `tooltips` explain
#' themselves on hover, and the weight column, when there is one, shows its
#' value as a bar.
#' @param data A data frame.
#' @param lang Character string language, as for [table_lang()].
#' @param tooltips Optional named character vector of column descriptions.
#' @param weight Optional character string name of the weight column.
#' @param ... Further arguments passed to [reactable::reactable()].
#' @return A reactable widget.
#' @keywords internal
app_table <- function(data, lang = "english", tooltips = NULL, weight = NULL, ...) {
  columns <- lapply(stats::setNames(nm = names(data)), function(name) {
    tip <- if (!is.null(tooltips)) tooltips[name] else NA
    header <- if (!is.na(tip)) {
      function(value) span(class = "ssd-has-tip", title = unname(tip), value)
    }
    cell <- if (identical(name, weight)) {
      function(value) {
        div(
          class = "ssd-weight",
          div(class = "ssd-weight-bar", style = sprintf("width: %.0f%%", 100 * max(value, 0.02))),
          span(format(value))
        )
      }
    }
    args <- Filter(Negate(is.null), list(header = header, cell = cell, minWidth = if (identical(name, weight)) 140))
    do.call(reactable::colDef, args)
  })
  reactable::reactable(
    data,
    columns = columns,
    highlight = TRUE,
    outlined = TRUE,
    compact = TRUE,
    theme = app_table_theme(),
    language = table_lang(lang),
    ...
  )
}

#' Create a step's page header
#'
#' The step's title, with its next action at the right. The title wraps beside
#' the action, which drops below it on narrow screens.
#' @param title Title tag.
#' @param action Optional action, such as a Continue button.
#' @return A div.
#' @keywords internal
page_header <- function(title, action = NULL) {
  div(
    class = "d-flex flex-wrap align-items-center justify-content-between gap-3 mb-4",
    div(class = "ssd-page-header-text", h1(class = "ssd-page-title", title)),
    if (!is.null(action)) div(class = "flex-shrink-0", action)
  )
}

#' Create a panel
#'
#' A card with a title, optional muted description and an action at the
#' right of the title.
#' @param title Title tag.
#' @param ... Panel content.
#' @param description Optional description under the title.
#' @param action Optional action tag.
#' @param class Optional character string of extra card classes.
#' @return A bslib card.
#' @keywords internal
panel <- function(title, ..., description = NULL, action = NULL, class = NULL) {
  card(
    # The panels of a step are spaced by the flex gap around them.
    class = paste(c("mb-0", class), collapse = " "),
    full_screen = FALSE,
    card_body(
      gap = "1rem",
      div(
        class = "d-flex align-items-start justify-content-between gap-3",
        div(
          h2(class = "ssd-card-title mb-0", title),
          if (!is.null(description)) div(class = "small text-body-secondary mt-1", description)
        ),
        if (!is.null(action)) div(class = "flex-shrink-0", action)
      ),
      ...
    )
  )
}

#' Lay out a step
#'
#' The step's settings in an aside beside its content on large screens, and
#' below it on small ones, so a phone shows the content first.
#' @param aside Aside content.
#' @param main Main content.
#' @return A Bootstrap row.
#' @keywords internal
step_layout <- function(aside, main) {
  div(
    class = "row g-4",
    div(class = "col-lg-4 order-last order-lg-first", div(class = "ssd-aside", aside)),
    div(class = "col-lg-8", main)
  )
}

#' Create a section of a step's aside
#' @param title Section title tag, shown as a small uppercase eyebrow.
#' @param ... Section content.
#' @return A div.
#' @keywords internal
aside_section <- function(title, ...) {
  div(
    class = "d-flex flex-column gap-2 mb-4",
    div(class = "ssd-eyebrow text-body-secondary", title),
    ...
  )
}

#' Create a button in one of the app's variants
#'
#' Primary (filled) is the next step of the analysis, at most one per screen.
#' Soft (tinted) is an optional step, such as Get CL. Outline is any other
#' action: navigation, Cancel and downloads. Ghost (no border) is a small
#' adjustment inside a panel. Each non-primary variant carries `btn-light`,
#' which keeps Shiny's `btn-default` styles off the button.
#' @param id Character string input ID.
#' @param label Button label.
#' @param icon Optional icon: a [lucide()] name or tag, shown before the label.
#' @param variant Character string: `"primary"`, `"soft"`, `"outline"` or
#'   `"ghost"`.
#' @param size Optional character string Bootstrap size: `"sm"` or `"lg"`.
#' @param class Optional character string of extra classes.
#' @param download Logical scalar: whether the button downloads the file of
#'   the [shiny::downloadHandler()] output `id`.
#' @param busy Logical scalar: whether a download button shows a spinner, and
#'   is disabled, from its click until [download_done()] is called for it. For
#'   a file that takes some seconds to prepare.
#' @param ... Further arguments passed to [shiny::actionButton()] or
#'   [shiny::downloadLink()].
#' @return A button tag.
#' @keywords internal
button <- function(
  id,
  label,
  icon = NULL,
  variant = c("primary", "soft", "outline", "ghost"),
  size = NULL,
  class = NULL,
  download = FALSE,
  busy = FALSE,
  ...
) {
  variant <- match.arg(variant)
  classes <- c(
    switch(
      variant,
      primary = "btn-primary",
      soft = "btn-light ssd-btn-soft",
      outline = "btn-light border",
      ghost = "btn-light"
    ),
    if (!is.null(size)) paste0("btn-", size),
    class
  )
  if (is.character(icon) && !inherits(icon, "html")) icon <- lucide(icon)
  if (busy) {
    # Both icons, so the spinner can replace the icon in the browser; in the
    # button's text colour, as its icon is.
    icon <- tagList(span(class = "ssd-idle-icon", icon), span(class = "ssd-busy-icon", lucide("loader-2", "ssd-spin")))
  }
  label <- span(class = "d-inline-flex align-items-center gap-2", icon, label)
  if (download) {
    return(downloadLink(
      id,
      label,
      class = paste(c("btn btn-default", classes), collapse = " "),
      `data-busy-download` = if (busy) "",
      ...
    ))
  }
  actionButton(id, label, class = paste(classes, collapse = " "), ...)
}

#' Create a notice
#' @param icon Icon: a [lucide()] name or tag.
#' @param title Notice title.
#' @param ... Optional body content.
#' @param tone Character string: `"info"`, `"warning"`, `"danger"` or
#'   `"muted"`.
#' @param action Optional tag shown on the right, such as a button.
#' @return A div with the notice; errors are announced at once by screen
#'   readers (role alert), other notices when they are idle (role status).
#' @keywords internal
notice <- function(
  icon,
  title,
  ...,
  tone = c("info", "warning", "danger", "muted"),
  action = NULL
) {
  tone <- match.arg(tone)
  box <- switch(
    tone,
    muted = "bg-body-tertiary",
    sprintf("bg-%s-subtle border-%s-subtle", tone, tone)
  )
  icon_class <- if (tone == "muted") "text-body-secondary" else paste0("text-", tone)
  if (is.character(icon) && !inherits(icon, "html")) icon <- lucide(icon)
  body <- Filter(Negate(is.null), list(...))
  div(
    role = if (tone == "danger") "alert" else "status",
    class = paste("d-flex align-items-start gap-3 rounded-3 border p-3 small", box),
    span(class = paste("ssd-notice-icon", icon_class), icon),
    div(
      class = "flex-grow-1 d-flex flex-column gap-1",
      div(class = "fw-semibold", title),
      if (length(body) > 0) div(class = "text-body-secondary", body)
    ),
    if (!is.null(action)) div(class = "flex-shrink-0", action)
  )
}

#' Create a busy icon
#' @return A spinning loader icon, hidden from screen readers (the text beside
#'   it says what is running).
#' @keywords internal
busy_icon <- function() {
  lucide("loader-2", "ssd-spin text-primary")
}

#' Mark a download as done
#'
#' Ends the spinner of a download [button()] with `busy = TRUE`; called when
#' the [shiny::downloadHandler()]'s content function exits.
#' @param session The module's session.
#' @param id Character string output ID, without the namespace.
#' @return Called for its side effect.
#' @keywords internal
download_done <- function(session, id) {
  session$sendCustomMessage("downloadDone", session$ns(id))
}

#' Create an empty state
#'
#' Shown in place of a step that needs an earlier one: what is missing, and a
#' button to go there.
#' @param icon Icon: a [lucide()] name or tag.
#' @param title Short title.
#' @param description Optional longer description.
#' @param action Optional button.
#' @return A card with the empty state.
#' @keywords internal
empty_state <- function(icon, title, description = NULL, action = NULL) {
  if (is.character(icon) && !inherits(icon, "html")) icon <- lucide(icon)
  card(
    class = "mb-0",
    card_body(
      class = "d-flex flex-column align-items-center text-center gap-2 py-5",
      div(class = "ssd-empty-icon bg-primary-subtle text-primary-emphasis", icon),
      div(class = "fw-semibold mt-1", title),
      if (!is.null(description)) div(class = "text-body-secondary ssd-measure", description),
      if (!is.null(action)) div(class = "mt-2", action)
    )
  )
}

# The steps of an analysis, for the welcome card: the translation keys of
# each step's name and of its one-line description.
app_steps <- list(
  list(name = "ui_nav1", name_text = "1. Data", key = "ui_step_data", text = "Use the boron dataset, upload a CSV file or fill out a table."),
  list(name = "ui_nav2", name_text = "2. Fit", key = "ui_step_fit", text = "Fit distributions to the concentrations and compare how well they fit."),
  list(name = "ui_nav3", name_text = "3. Predict", key = "ui_step_predict", text = "Estimate a hazard concentration or the fraction affected, with confidence limits."),
  list(name = "ui_navexport", name_text = "4. Export", key = "ui_step_export", text = "Download the plots, tables and BCANZ report, and the R code that reproduces them.")
)

#' Create the welcome card
#'
#' The app's purpose and its steps, shown on the Data step until data are
#' loaded.
#' @param guide_id Character string namespaced input ID of the link to the
#'   User Guide.
#' @return A card.
#' @keywords internal
welcome_card <- function(guide_id) {
  step <- function(item, number) {
    div(
      class = "col-sm-6 col-xl-3 d-flex align-items-start gap-2",
      span(class = "ssd-step-marker ssd-step-todo flex-shrink-0 mt-1", `aria-hidden` = "true", number),
      div(
        div(class = "fw-semibold mb-1", step_name(item$name, item$name_text)),
        div(class = "small text-body-secondary", span(`data-translate` = item$key, item$text))
      )
    )
  }
  card(
    class = "mb-0",
    card_body(
      gap = "1rem",
      div(
        class = "d-flex align-items-center gap-3",
        div(class = "ssd-tile-icon bg-primary-subtle text-primary-emphasis", ssd_art("ssd-art-sm")),
        div(
          h2(class = "ssd-card-title mb-0", "ssdtools"),
          div(class = "small text-body-secondary", span(`data-translate` = "ui_navtitle", "Fit and Plot Species Sensitivity Distributions"))
        )
      ),
      div(class = "row g-3", Map(step, app_steps, seq_along(app_steps))),
      div(
        actionLink(
          guide_id,
          span(
            class = "d-inline-flex align-items-center gap-2",
            lucide("book-open"),
            span(`data-translate` = "ui_navguide", "User Guide")
          )
        )
      )
    )
  )
}

#' Create a button that opens a step
#'
#' The Continue button of a step (primary, in its page header), or the button
#' of an empty state that opens the step it needs (outline). Its server side
#' is [observe_step_button()].
#' @param id Character string namespaced input ID.
#' @param translate_key Character string translation key of the label.
#' @param default_text Character string English label.
#' @param variant Character string button variant, as for [button()].
#' @return A button tag.
#' @keywords internal
step_button <- function(id, translate_key, default_text, variant = "primary") {
  button(
    id,
    span(`data-translate` = translate_key, default_text),
    icon = "arrow-right",
    variant = variant
  )
}

#' Open a step when its button is clicked
#' @param input The module's input.
#' @param id Character string input ID of the button.
#' @param step Character string value of the step's tab in `main_nav`.
#' @param session The module's session.
#' @return An observer.
#' @keywords internal
observe_step_button <- function(input, id, step, session = getDefaultReactiveDomain()) {
  observe(nav_select("main_nav", step, session = session$rootScope())) |>
    bindEvent(input[[id]])
}
