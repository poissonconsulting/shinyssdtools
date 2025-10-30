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

# Script to extract helpfile sections from user guide markdown files
# This ensures helpfiles and user guide stay in sync with one source of truth

#' Extract a section from markdown file
#' @param file_path Path to the markdown file
#' @param section_heading The heading to extract (e.g., "Step 1: Provide data")
#' @param output_heading The heading for the output file (e.g., "Provide data")
#' @return Character vector of the extracted section
extract_section <- function(file_path, section_heading, output_heading) {
  lines <- readLines(file_path, encoding = "UTF-8")

  # Find the start line (section heading)
  # Use fixed=TRUE to avoid regex issues
  search_string <- paste0("### ", section_heading)
  start_line <- which(trimws(lines) == search_string)[1]

  if (is.na(start_line)) {
    stop(sprintf("Section '%s' not found in %s", section_heading, file_path))
  }

  # Find the next heading (end of section)
  next_heading <- grep("^###\\s+", lines)
  end_line <- next_heading[next_heading > start_line][1]

  if (is.na(end_line)) {
    # If no next heading, go to end of file
    end_line <- length(lines)
  } else {
    # Stop before the next heading
    end_line <- end_line - 1
  }

  # Extract the section (excluding the original heading)
  section_lines <- lines[(start_line + 1):end_line]

  # Remove trailing empty lines
  while (
    length(section_lines) > 0 &&
      trimws(section_lines[length(section_lines)]) == ""
  ) {
    section_lines <- section_lines[-length(section_lines)]
  }

  # Add the output heading
  result <- c(paste0("## ", output_heading), "", section_lines)

  return(result)
}

#' Build helpfiles for a specific language
#' @param lang Language code: "en", "fr", "es", or "ja"
build_helpfiles_for_language <- function(lang) {
  user_guide_path <- file.path("inst/extdata", paste0("user-", lang, ".md"))
  helpfiles_dir <- file.path("inst", paste0("helpfiles_", lang))

  # Check if user guide exists
  if (!file.exists(user_guide_path)) {
    warning(sprintf("User guide not found: %s", user_guide_path))
    return(invisible(NULL))
  }

  # Create helpfiles directory if it doesn't exist
  if (!dir.exists(helpfiles_dir)) {
    dir.create(helpfiles_dir, recursive = TRUE)
  }

  # Language-specific section and output headings
  sections <- switch(
    lang,
    "en" = list(
      dataTab = list(
        section = "Step 1: Provide data",
        output = "Provide data"
      ),
      fitTab = list(
        section = "Step 2: Fit distributions",
        output = "Fit distributions"
      ),
      predictTab = list(
        section = "Step 3: Predict hazard concentration or percent of species effected",
        output = "Estimate hazard concentration"
      ),
      reportTab = list(
        section = "Step 4: Get BCANZ report",
        output = "Get BCANZ report"
      ),
      rcodeTab = list(
        section = "Step 5: Get R code",
        output = "Get R code"
      )
    ),
    "fr" = list(
      dataTab = list(
        section = "Étape 1: Fournir les données",
        output = "Fournir les données"
      ),
      fitTab = list(
        section = "Étape 2: Ajustement des distributions",
        output = "Ajustement des distributions"
      ),
      predictTab = list(
        section = "Étape 3: Estimation de la concentration présentant un risque ou du pourcentage d'espèces affectées",
        output = "Estimation de la concentration présentant un risque"
      ),
      reportTab = list(
        section = "Étape 4: Obtenir le BCANZ rapport",
        output = "Obtenir le BCANZ rapport"
      ),
      rcodeTab = list(
        section = "Étape 5: Obtenir le code R",
        output = "Obtenir le code R"
      )
    ),
    "es" = list(
      dataTab = list(
        section = "Paso 1: Proporcionar datos",
        output = "Proporcionar datos"
      ),
      fitTab = list(
        section = "Paso 2: Ajustar distribuciones",
        output = "Ajustar distribuciones"
      ),
      predictTab = list(
        section = "Paso 3: Predecir concentración de peligro o porcentaje de especies afectadas",
        output = "Estimar concentración de peligro"
      ),
      reportTab = list(
        section = "Paso 4: Obtener informe BCANZ",
        output = "Obtener informe BCANZ"
      ),
      rcodeTab = list(
        section = "Paso 5: Obtener código R",
        output = "Obtener código R"
      )
    ),
    "ja" = list(
      dataTab = list(
        section = "ステップ1: データの提供",
        output = "データの提供"
      ),
      fitTab = list(
        section = "ステップ2: 分布の適合",
        output = "分布の適合"
      ),
      predictTab = list(
        section = "ステップ3: ハザード濃度または影響を受けた種のパーセンテージの予測",
        output = "ハザード濃度の推定"
      ),
      reportTab = list(
        section = "ステップ4: BCANZレポートの取得",
        output = "BCANZレポートの取得"
      ),
      rcodeTab = list(
        section = "ステップ5: Rコードの取得",
        output = "Rコードの取得"
      )
    )
  )

  # Extract and write each helpfile
  for (tab_name in names(sections)) {
    section_info <- sections[[tab_name]]
    output_file <- file.path(helpfiles_dir, paste0(tab_name, ".md"))

    tryCatch(
      {
        content <- extract_section(
          user_guide_path,
          section_info$section,
          section_info$output
        )
        writeLines(content, output_file, useBytes = TRUE)
        message(sprintf("Created: %s", output_file))
      },
      error = function(e) {
        warning(sprintf("Failed to create %s: %s", output_file, e$message))
      }
    )
  }
}

# Build helpfiles for all languages
message("Building helpfiles from user guides...")
message("=", paste(rep("=", 50), collapse = ""))

for (lang in c("en", "fr", "es", "ja")) {
  message(sprintf("\nBuilding helpfiles for language: %s", lang))
  build_helpfiles_for_language(lang)
}

message("\n", paste(rep("=", 50), collapse = ""), "=")
message("Helpfiles build complete!")
