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

# The app's theme: colours, radii, spacing and component looks are Bootstrap
# Sass variables, so custom CSS (inst/app/www/style.css) only styles the app's
# own ssd-* classes and reads colours from var(--bs-*).
#
# The colours meet WCAG 2.1 AA contrast for text on white (primary) and for
# white text on the navbar (secondary).

color_primary <- "#2e7d9a" # buttons, links, active tabs and help icons
color_secondary <- "#1e3a5f" # navbar
color_sidebar <- "#f4f6f9" # step navigation
color_button_icon <- "text-white" # icons on primary buttons

# Card styling
card_shadow <- "border"

app_theme <- function() {
  bs_theme(
    version = 5,
    primary = color_primary,
    secondary = "#5a6672",
    success = "#187c49",
    info = color_primary,
    warning = "#a76100",
    danger = "#c5221f",
    "font-size-base" = "0.9375rem",
    "headings-font-weight" = 600,
    "border-radius" = "0.5rem",
    "border-radius-sm" = "0.375rem",
    "border-radius-lg" = "0.625rem",
    "link-decoration" = "none",
    "link-hover-decoration" = "underline",
    "btn-font-weight" = 500,
    "card-cap-bg" = "transparent",
    "card-cap-padding-y" = "0.75rem",
    "form-label-font-weight" = 500,
    # The language menu drops down from the dark navbar, so it shares its colour.
    "dropdown-bg" = color_secondary,
    "dropdown-border-color" = color_secondary,
    "dropdown-link-color" = "#ffffff",
    "dropdown-link-hover-color" = "#ffffff",
    "dropdown-link-hover-bg" = "rgba(255, 255, 255, 0.1)",
    "dropdown-link-active-bg" = color_primary,
    "popover-max-width" = "22rem",
    "accordion-button-active-bg" = color_sidebar,
    "accordion-button-active-color" = "#212529",
    "progress-height" = "0.5rem"
  )
}
