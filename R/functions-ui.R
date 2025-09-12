ui_waiter <- function(id, ns){
  waiter::Waiter$new(id = ns(id), html = waiter::spin_2(), color = "white")
}

dt_options <- function(lang = "english"){
  # Language-specific translations
  lang_options <- if (lang == "french") {
    list(
      search = "Rechercher :",
      lengthMenu = "Afficher _MENU_ entrées",
      info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
      infoEmpty = "Aucune donnée disponible",
      infoFiltered = "(filtré à partir de _MAX_ entrées au total)",
      zeroRecords = "Aucun enregistrement correspondant trouvé",
      paginate = list(
        first = "Premier",
        last = "Dernier",
        `next` = "Suivant",
        previous = "Précédent"
      ),
      processing = "Traitement en cours...",
      loadingRecords = "Chargement des enregistrements...",
      emptyTable = "Aucune donnée disponible dans le tableau"
    )
  } else {
    list(
      search = "Search data:",
      lengthMenu = "Show _MENU_ entries",
      info = "Showing _START_ to _END_ of _TOTAL_ entries",
      infoEmpty = "No data available",
      infoFiltered = "(filtered from _MAX_ total entries)",
      zeroRecords = "No matching records found",
      paginate = list(
        first = "First",
        last = "Last",
        `next` = "Next",
        previous = "Previous"
      ),
      processing = "Processing...",
      loadingRecords = "Loading records...",
      emptyTable = "No data available in table"
    )
  }
  
  list(
    # Pagination and display
    pageLength = 15,
    lengthMenu = c(10, 15, 25, 50, 100),
    # Search and filtering
    searchHighlight = TRUE,
    search = list(regex = TRUE, caseInsensitive = TRUE),
    
    # Column features
    columnDefs = list(
      list(className = 'dt-center', targets = '_all'),
      list(searchable = TRUE, targets = '_all')
    ),
    
    # Language support
    language = lang_options
  )
}

ui_dashbox <- function(x){
  div(
    class = "text-muted text-center p-5",
    style = "border: 2px dashed #dee2e6; border-radius: 8px; margin: 2rem;",
    x
  )
}

ui_download_popover_table <- function(tab = "fit", ns){
  bslib::popover(
    actionButton(ns(paste0(tab, "DownloadBtnTbl")), 
                 label = tagList(bsicons::bs_icon("download"), span(`data-translate` = "ui_2download", "Download")),
                 style = "padding:4px; font-size:80%"
    ),
    card(
      style = "width: 250px; margin-top: 10px;",
      card_body(
        div(
          style = "display: grid; gap: 8px;",
          downloadButton(ns(paste0(tab, "DlXlsx")),
                         label = span(`data-translate` = "ui_2dlxlsx", "XLSX file"),
                         style = "width: 100%; padding: 6px; font-size: 12px;",
                         class = "btn-primary btn-sm"
          ),
          downloadButton(ns(paste0(tab, "DlCsv")),
                         label = span(`data-translate` = "ui_2dlcsv", "CSV File"),
                         style = "width: 100%; padding: 6px; font-size: 12px;",
                         class = "btn-primary btn-sm"
          )
        )
      )
    ),
    placement = "bottom"
  )
}

ui_download_popover <- function(tab = "fit", ns){
  bslib::popover(
    actionButton(ns(paste0(tab, "DownloadBtn")), 
                 label = tagList(bsicons::bs_icon("download"), span(`data-translate` = "ui_2download", "Download")),
                 style = "padding:4px; font-size:80%"
    ),
    card(
      style = "width: 250px; margin-top: 10px;",
      card_body(
        div(
          style = "display: grid; gap: 8px;",
          downloadButton(ns(paste0(tab, "DlPlot")),
                         label = span(`data-translate` = "ui_2dlplot", "PNG file"),
                         style = "width: 100%; padding: 6px; font-size: 12px;",
                         class = "btn-primary btn-sm"
          ),
          downloadButton(ns(paste0(tab, "DlRds")),
                         label = span(`data-translate` = "ui_2dlrds", "RDS File"),
                         style = "width: 100%; padding: 6px; font-size: 12px;",
                         class = "btn-primary btn-sm"
          )
        ),
        div(
          h6(span(`data-translate` = "ui_2png", "PNG Format Settings"), style = "margin-bottom: 10px;"),
          div(
            style = "display: flex; gap: 5px; justify-content: space-between;",
            div(
              style = "flex: 1; min-width: 0;",
              numericInput(ns("width2"), 
                           label = span(`data-translate` = "ui_2width", "Width"), 
                           value = 6, min = 1, max = 50, step = 1
              )
            ),
            div(
              style = "flex: 1; min-width: 0;",
              numericInput(ns("height2"), 
                           label = span(`data-translate` = "ui_2height", "Height"),
                           value = 4, min = 1, max = 50, step = 1
              )
            ),
            div(
              style = "flex: 1; min-width: 0;",
              numericInput(ns("dpi2"), 
                           label = span(`data-translate` = "ui_2dpi", "DPI"),
                           value = 300, min = 50, max = 2000, step = 50
              )
            )
          )
        )
      )
    ),
    placement = "bottom"
  )
}
