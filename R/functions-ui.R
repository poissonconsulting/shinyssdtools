dt_options <- function(lang = "English"){
  # Language-specific translations
  lang_options <- if (lang == "French") {
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