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

# The About page of the Help tab, built from the about-<language>.html files
# (rendered from inst/extdata/about-<language>.md, whose sections carry the
# ids methods, gof, cite and issues) so its text stays translated.

#' Split rendered About HTML into its sections
#' @param html Character string HTML fragment rendered by rmarkdown, with
#'   level 2 sections.
#' @return A list of `intro` (the HTML before the first section) and
#'   `sections`, each a list of `id`, `title` and `body` HTML.
#' @keywords internal
about_sections <- function(html) {
  pattern <- '(?s)<div id="([^"]+)" class="section level2">\\s*<h2>(.*?)</h2>(.*?)</div>'
  matches <- regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1]]
  sections <- lapply(matches, function(match) {
    parts <- regmatches(match, regexec(pattern, match, perl = TRUE))[[1]]
    list(id = parts[2], title = parts[3], body = parts[4])
  })
  intro <- sub('(?s)<div id="[^"]+" class="section level2">.*$', "", html, perl = TRUE)
  list(intro = intro, sections = stats::setNames(sections, vapply(sections, `[[`, "", "id")))
}

# A citation block with a Copy button for its text.
about_citation <- function(text) {
  onclick <- paste0(
    "navigator.clipboard.writeText(this.closest('.ssd-cite').querySelector('.ssd-cite-text').innerText)",
    ".then(() => { this.classList.add('ssd-copied'); setTimeout(() => this.classList.remove('ssd-copied'), 1500); })"
  )
  div(
    class = "ssd-cite d-flex align-items-start gap-2 rounded-3 bg-body-tertiary p-3 small",
    div(class = "ssd-cite-text flex-grow-1", HTML(text)),
    tags$button(
      type = "button",
      class = "btn btn-light border btn-sm flex-shrink-0",
      onclick = onclick,
      title = "Copy",
      `aria-label` = "Copy",
      span(class = "ssd-copy-icon", lucide("copy")),
      span(class = "ssd-copied-icon", lucide("check"))
    )
  )
}

#' Create the About page
#' @param html Character string rendered About HTML for the current language.
#' @return The page's content.
#' @keywords internal
about_page <- function(html) {
  about <- about_sections(html)
  sections <- about$sections
  version <- function(package) as.character(utils::packageVersion(package))
  fact <- function(...) span(class = "badge border text-body-secondary fw-medium", ...)
  link <- function(href, icon, text) {
    tags$a(
      href = href, target = "_blank", rel = "noopener",
      class = "btn btn-light border btn-sm",
      span(class = "d-inline-flex align-items-center gap-2", lucide(icon), text)
    )
  }
  body <- function(id) {
    html <- sections[[id]]$body
    if (id == "methods") {
      # Each article as a tile: its title as the link, its summary below.
      html <- gsub("</a>\\s*-\\s*", "</a><br>", html)
    }
    if (id == "cite") {
      parts <- strsplit(html, "(?s)<blockquote>\\s*<p>|</p>\\s*</blockquote>", perl = TRUE)[[1]]
      # Prefaces and citations alternate.
      return(div(
        class = "d-flex flex-column gap-2",
        lapply(seq_along(parts), function(i) {
          if (i %% 2 == 0) about_citation(parts[[i]]) else if (nzchar(trimws(parts[[i]]))) HTML(parts[[i]])
        })
      ))
    }
    HTML(html)
  }
  section_panel <- function(id) {
    if (is.null(sections[[id]])) {
      return(NULL)
    }
    panel(HTML(sections[[id]]$title), div(class = paste0("ssd-about ssd-about-", id), body(id)))
  }
  issues <- sections$issues

  div(
    class = "d-flex flex-column gap-4",
    card(
      class = "mb-0",
      card_body(
        gap = "1rem",
        div(class = "ssd-about text-body-secondary", HTML(about$intro)),
        div(
          class = "d-flex flex-wrap align-items-center gap-2",
          fact("shinyssdtools ", version("shinyssdtools")),
          fact("ssdtools ", version("ssdtools")),
          fact("Apache-2.0"),
          span(class = "flex-grow-1"),
          link("https://bcgov.github.io/ssdtools/", "book-open", "ssdtools"),
          link("https://github.com/poissonconsulting/shinyssdtools", "github", "GitHub")
        )
      )
    ),
    section_panel("methods"),
    section_panel("cite"),
    section_panel("gof"),
    if (!is.null(issues)) {
      card(
        class = "mb-0",
        card_body(div(
          class = "d-flex align-items-center gap-3",
          div(class = "ssd-tile-icon bg-primary-subtle text-primary-emphasis", lucide("github")),
          div(
            class = "flex-grow-1",
            h2(class = "ssd-card-title mb-1", HTML(issues$title)),
            div(class = "ssd-about small text-body-secondary", HTML(issues$body))
          )
        ))
      )
    }
  )
}
