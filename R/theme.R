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

# The app's theme: one of several palettes (app_palettes), each a neutral
# family for the page and an accent for primary actions, expressed as
# Bootstrap Sass variables. Everything else reads colours from var(--bs-*)
# rather than from a palette; custom CSS (inst/app/www/style.css) styles only
# the app's own ssd-* classes.
#
# Every text and border pair meets WCAG 2.1 AA contrast (input borders 3:1,
# text 4.5:1, white text on the primary 4.5:1).

neutrals <- list(
  slate = list(bg = "#f8fafc", fg = "#0f172a", muted = "#f1f5f9", muted_fg = "#64748b", border = "#e2e8f0", input = "#8291a6", secondary_fg = "#334155"),
  zinc = list(bg = "#fafafa", fg = "#18181b", muted = "#f4f4f5", muted_fg = "#71717a", border = "#e4e4e7", input = "#85858f", secondary_fg = "#3f3f46"),
  # kelpbioshiny's greys, from the Hakai Institute's pkgdown site.
  hakai = list(bg = "#f6f7f9", fg = "#1d2733", muted = "#eef1f4", muted_fg = "#5a6672", border = "#e1e5ea", input = "#8a949e", secondary_fg = "#2c3e50"),
  stone = list(bg = "#fafaf9", fg = "#1c1917", muted = "#f5f5f4", muted_fg = "#78716c", border = "#e7e5e4", input = "#8a827a", secondary_fg = "#44403c")
)

accent <- function(primary, subtle, border, emphasis, ring_rgb) {
  list(primary = primary, accent = subtle, accent_border = border, accent_fg = emphasis, ring_rgb = ring_rgb)
}

app_palettes <- list(
  hakai = c(neutrals$hakai, accent("#2c3e50", "#eef2f6", "#cbd5e1", "#1e2b38", "71, 97, 122"), list(label = "Hakai slate", navbar_bg = "#2c3e50")),
  indigo = c(neutrals$slate, accent("#4f46e5", "#eef2ff", "#c7d2fe", "#3730a3", "99, 102, 241"), list(label = "Indigo", navbar_bg = "#0f172a")),
  ocean = c(neutrals$slate, accent("#0f766e", "#f0fdfa", "#99f6e4", "#115e59", "20, 184, 166"), list(label = "Ocean", navbar_bg = "#0b1f2a")),
  graphite = c(neutrals$zinc, accent("#0969da", "#ddf4ff", "#b6e3ff", "#0550ae", "84, 174, 255"), list(label = "Graphite", navbar_bg = "#18181b")),
  forest = c(neutrals$stone, accent("#15803d", "#f0fdf4", "#bbf7d0", "#166534", "34, 197, 94"), list(label = "Forest", navbar_bg = "#1a2e22")),
  ember = c(neutrals$stone, accent("#c2410c", "#fff7ed", "#fed7aa", "#9a3412", "249, 115, 22"), list(label = "Ember", navbar_bg = "#1c1917")),
  violet = c(neutrals$zinc, accent("#7c3aed", "#f5f3ff", "#ddd6fe", "#5b21b6", "139, 92, 246"), list(label = "Violet", navbar_bg = "#1e1b2e"))
)

app_status_colours <- list(
  success = "#047857", success_muted = "#d1fae5",
  warning = "#b45309", warning_fg = "#92400e", warning_muted = "#fef3c7", warning_border = "#fde68a",
  info = "#0369a1", info_bg = "#e0f2fe", info_border = "#bae6fd", info_fg = "#075985",
  danger = "#dc2626"
)

# The shinyhelper help icons take the theme's primary colour.
color_primary <- "var(--bs-primary)"

# `navbar` is "dark" (the palette's navbar colour with white text) or "light"
# (white with a hairline border and the page's text colour).
app_theme <- function(palette = "hakai", navbar = "light") {
  col <- c(app_palettes[[palette]], app_status_colours)
  col$card <- "#ffffff"
  col$ring <- sprintf("rgb(%s)", col$ring_rgb)
  light <- navbar == "light"
  nav <- if (light) {
    list(bg = col$card, fg = col$muted_fg, hover = col$fg, active = col$fg, brand = col$fg)
  } else {
    list(bg = col$navbar_bg, fg = "rgba(255, 255, 255, 0.72)", hover = "#ffffff", active = "#ffffff", brand = "#ffffff")
  }
  # A done step's marker: a white disc on the dark navbar, a green one on the
  # light navbar. Compiled with the theme, so it follows a theme change.
  rules <- c(
    sprintf(".ssd-step-done { background: %s; color: %s; }", if (light) col$success_muted else "#ffffff", if (light) col$success else col$secondary_fg),
    sprintf(".ssd-step-busy { background: %s; color: %s; }", col$accent, col$accent_fg),
    if (light) sprintf(".navbar { border-bottom: 1px solid %s; }", col$border)
  )
  theme <- bs_theme(
    version = 5,
    bg = col$bg,
    fg = col$fg,
    primary = col$primary,
    secondary = col$muted_fg,
    success = col$success,
    warning = col$warning,
    danger = col$danger,
    info = col$info,
    light = "#ffffff",
    base_font = font_collection(font_google("Inter", wght = "400..700", local = FALSE), "system-ui", "sans-serif"),
    code_font = font_collection("ui-monospace", "SFMono-Regular", "Menlo", "Consolas", "monospace"),
    "font-size-base" = "0.875rem",
    "line-height-base" = 1.5,
    "headings-font-weight" = 600,
    "body-secondary-color" = col$muted_fg,
    "body-tertiary-bg" = col$muted,
    "border-color" = col$border,
    "border-radius" = "0.5rem",
    "border-radius-sm" = "0.375rem",
    "border-radius-lg" = "0.625rem",
    "border-radius-xl" = "0.875rem",
    "box-shadow-sm" = sprintf("0 0 0 1px %s, 0 1px 2px rgba(15, 23, 42, 0.05)", col$border),
    "link-color" = col$primary,
    "link-decoration" = "none",
    "link-hover-decoration" = "underline",
    "code-color" = col$fg,
    "primary-bg-subtle" = col$accent,
    "primary-border-subtle" = col$accent_border,
    "primary-text-emphasis" = col$accent_fg,
    "secondary-bg-subtle" = col$muted,
    "secondary-text-emphasis" = col$secondary_fg,
    "info-bg-subtle" = col$info_bg,
    "info-border-subtle" = col$info_border,
    "info-text-emphasis" = col$info_fg,
    "success-bg-subtle" = col$success_muted,
    "success-text-emphasis" = col$success,
    "warning-bg-subtle" = col$warning_muted,
    "warning-border-subtle" = col$warning_border,
    "warning-text-emphasis" = col$warning_fg,
    "card-bg" = col$card,
    "card-border-radius" = "0.875rem",
    "card-spacer-y" = "1.5rem",
    "card-spacer-x" = "1.5rem",
    "card-cap-bg" = "transparent",
    "btn-font-weight" = 500,
    "btn-padding-y" = "0.4375rem",
    "btn-padding-x" = "0.875rem",
    "btn-hover-bg-shade-amount" = "8%",
    "btn-active-bg-shade-amount" = "12%",
    "input-btn-font-size" = "0.875rem",
    "input-bg" = col$card,
    "input-border-color" = col$input,
    "form-check-input-border" = sprintf("1px solid %s", col$input),
    "input-focus-border-color" = col$ring,
    "input-focus-box-shadow" = sprintf("0 0 0 3px rgba(%s, 0.25)", col$ring_rgb),
    "form-label-font-weight" = 500,
    "badge-font-size" = "0.75rem",
    "badge-font-weight" = 500,
    "progress-bg" = col$muted,
    "progress-height" = "0.5rem",
    "navbar-bg" = nav$bg,
    # The underlined step tabs get extra bottom padding equal to the navbar's,
    # so equal top padding keeps their labels on the navbar's centre line.
    "navbar-padding-y" = "0.75rem",
    "nav-link-padding-y" = "0.75rem",
    "navbar-light-color" = nav$fg,
    "navbar-light-hover-color" = nav$hover,
    "navbar-light-active-color" = nav$active,
    "navbar-light-brand-color" = nav$brand,
    "navbar-light-brand-hover-color" = nav$brand,
    "nav-link-font-weight" = 500,
    "nav-link-color" = col$fg,
    "nav-link-hover-color" = col$fg,
    "nav-underline-gap" = "1.25rem",
    "nav-underline-link-active-color" = col$primary,
    "nav-pills-link-active-bg" = col$accent,
    "nav-pills-link-active-color" = col$accent_fg,
    # The language menu drops down from the dark navbar.
    "dropdown-bg" = col$card,
    "dropdown-link-active-bg" = col$accent,
    "dropdown-link-active-color" = col$accent_fg,
    "popover-max-width" = "22rem",
    "popover-header-bg" = col$card,
    "popover-border-color" = col$border,
    "accordion-bg" = col$card,
    "accordion-border-color" = col$border,
    "accordion-button-active-bg" = col$card,
    "accordion-button-active-color" = col$fg,
    "accordion-button-padding-y" = "0.875rem",
    "accordion-button-padding-x" = "1rem",
    "accordion-body-padding-x" = "1rem",
    "table-cell-padding-y" = "0.75rem",
    "table-cell-padding-x" = "0.75rem",
    "table-border-color" = col$border,
    "table-th-font-weight" = 500,
    "table-bg" = "transparent",
    "modal-content-border-radius" = "0.875rem"
  )
  bs_add_rules(theme, rules)
}

# reactable styles from the theme's CSS variables.
app_table_theme <- function() {
  reactable::reactableTheme(
    color = "var(--bs-body-color)",
    backgroundColor = "var(--bs-card-bg, var(--bs-body-bg))",
    borderColor = "var(--bs-border-color)",
    highlightColor = "var(--bs-tertiary-bg)",
    cellPadding = "0.5rem 0.75rem",
    style = list(fontFamily = "inherit", fontSize = "0.8125rem", fontVariantNumeric = "tabular-nums"),
    headerStyle = list(
      background = "var(--bs-tertiary-bg)",
      color = "var(--bs-secondary-color)",
      fontWeight = 500,
      borderBottomColor = "var(--bs-border-color)"
    ),
    searchInputStyle = list(
      borderColor = "var(--bs-border-color)",
      borderRadius = "0.375rem",
      width = "14rem"
    ),
    paginationStyle = list(color = "var(--bs-secondary-color)", borderTopColor = "var(--bs-border-color)"),
    pageButtonStyle = list(border = "1px solid var(--bs-border-color)", borderRadius = "0.375rem", margin = "0 0.125rem"),
    pageButtonHoverStyle = list(background = "var(--bs-primary-bg-subtle)")
  )
}
