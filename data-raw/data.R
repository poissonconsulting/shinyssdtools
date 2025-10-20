library(dplyr)

# All translations unified from CSV (previously split between Excel and manual tibbles)
translations <- dplyr::bind_rows(
  dplyr::tibble(
    id = "ui_navtitle",
    english = "Fit and Plot Species Sensitivity Distributions",
    french = "Ajustement et Représentation des courbes de Distribution de la Sensibilité des Espèces"
  ),
  dplyr::tibble(
    id = "ui_nav1",
    english = "1. Data",
    french = "1. Données"
  ),
  dplyr::tibble(
    id = "ui_nav2",
    english = "2. Fit",
    french = "2. Ajustement"
  ),
  dplyr::tibble(
    id = "ui_nav3",
    english = "3. Predict",
    french = "3. Estimation"
  ),
  dplyr::tibble(
    id = "ui_nav4",
    english = "4. Report",
    french = "4. Rapport"
  ),
  dplyr::tibble(
    id = "ui_nav5",
    english = "R Code",
    french = "Code R"
  ),
  dplyr::tibble(
    id = "ui_navanalyse",
    english = "Analyse",
    french = "Analyser"
  ),
  dplyr::tibble(
    id = "ui_navabout",
    english = "About",
    french = "À propos"
  ),
  dplyr::tibble(
    id = "ui_navguide",
    english = "User Guide",
    french = "Guide de l’utilisateur"
  ),
  dplyr::tibble(
    id = "ui_1choose",
    english = "Choose one of the following options:",
    french = "Choisir l’une des options suivantes :"
  ),
  dplyr::tibble(
    id = "ui_1data",
    english = "1. Use",
    french = "1. Utiliser"
  ),
  dplyr::tibble(
    id = "ui_1data2",
    english = "boron dataset",
    french = "l’ensemble de données pour le bore"
  ),
  dplyr::tibble(
    id = "ui_1datahelp",
    english = "1. This can be used to demo the app or view a dataset that 'works'.",
    french = "1. Ces données peuvent être téléchargées pour visualiser un ensemble 'fonctionnel' de données."
  ),
  dplyr::tibble(
    id = "ui_1csv",
    english = "2. Upload CSV file",
    french = "2. Télécharger un fichier CSV"
  ),
  dplyr::tibble(
    id = "ui_1csvhelp",
    english = "2. Upload a csv file containing your dataset. The dataset must include a column with at least 8 distinct, positive, non-missing, numeric concentration values. Other useful but optional variables include species and group, which may be used to label and color plot output, respectively. If you have an xls/xlsx file, try exporting a worksheet to csv using excel.",
    french = "2. Télécharger vos données en fichier .csv. L’ensemble de données doit contenir une colonne comportant au minimum 8 valeurs numériques de concentrations distinctes, positives et sans valeur manquantes. Il est possible d'ajouter des colonnes pour les espèces et les groupes taxonomiques, ce qui est pratique mais optionnel. Des étiquettes et des couleurs sont alors disponibles pour permettre leur identification dans le graphique. Si vous avez un fichier .xls/.xlsx, il faut l’exporter dans une feuille de calcul .csv en utilisant Excel."
  ),
  dplyr::tibble(
    id = "ui_1csvlabel",
    english = "Upload your data",
    french = "Télécharger les données"
  ),
  dplyr::tibble(
    id = "ui_1table",
    english = "3. Fill out table below:",
    french = "3. Remplir le tableau ci-dessous:"
  ),
  dplyr::tibble(
    id = "ui_1tablehelp",
    english = "3. The table below is interactive and acts like a simple excel spreadsheet. Click on a cell to begin data input. Right-click on the table to delete/insert rows or columns. Column names cannot be changed. The Concentration column must be filled out, with at least 8 distinct, positive, non-missing, numeric values. Species and Group are optional and may be used to format plot outputs. If the table is behaving unexpectedly, please reload the website.",
    french = "3. Le tableau ci-dessous est interactif et il s’utilise comme une feuille de calcul Excel normale. Cliquer sur une cellule pour commencer à entrer les données. Faire un clic-droit pour ajouter ou supprimer des colonnes ou des rangées. Il n'est pas possiible de modifier le nom des colonnes.  La colonne concentration doit être remplie avec au moins 8 valeurs numériques positives, distinctes et sans valeur manquante. Les colonnes Espèces et Groupes sont optionnelles et sont utilisées pour la mise en forme du graphique. Relancer le site web si des erreurs surviennent dans le tableau."
  ),
  dplyr::tibble(
    id = "ui_1note",
    english = "Note: the app is designed to handle one chemical at a time. Each species should not have more than one concentration value.",
    french = "Note : L’application est faite pour travailler un contaminant à la fois. Il n’est pas possible d’entrer plus d’une concentration par espèce."
  ),
  dplyr::tibble(
    id = "ui_1preview",
    english = "Preview chosen dataset",
    french = "Prévisualiser l'ensemble de données"
  ),
  dplyr::tibble(
    id = "ui_2conc",
    english = "Select column with concentration values",
    french = "Sélectionner la colonne avec les valeurs de concentration"
  ),
  dplyr::tibble(
    id = "ui_2dist",
    english = "Select distributions to fit",
    french = "Sélectionner les distributions pour l’ajustement"
  ),
  dplyr::tibble(
    id = "ui_2png",
    english = "PNG file formatting options",
    french = "Options de formatage du fichier PNG"
  ),
  dplyr::tibble(
    id = "ui_2plot",
    english = "Plot fitted distributions",
    french = "Représentation des courbes de distribution ajustées"
  ),
  dplyr::tibble(
    id = "ui_2table",
    english = "Goodness of Fit table",
    french = "Tableau de l'évaluation de la qualité de l’ajustement des courbes de distribution"
  ),
  dplyr::tibble(
    id = "ui_2weight",
    english = "weight",
    french = "coefficient de pondération"
  ),
  dplyr::tibble(
    id = "ui_2ploty",
    english = "Species affected (%)",
    french = "Pourcentage d’espèces affectées"
  ),
  dplyr::tibble(
    id = "ui_2download",
    english = "Download",
    french = "Télécharger"
  ),
  dplyr::tibble(
    id = "ui_2height",
    english = "Height",
    french = "Hauteur"
  ),
  dplyr::tibble(
    id = "ui_2width",
    english = "Width",
    french = "Largeur"
  ),
  dplyr::tibble(
    id = "ui_2dpi",
    english = "Dpi",
    french = "Dpi"
  ),
  dplyr::tibble(
    id = "ui_2dlpdf",
    english = "PDF File",
    french = "Fichier PDF"
  ),
  dplyr::tibble(
    id = "ui_2dlhtml",
    english = "HTML File",
    french = "Fichier HTML"
  ),
  dplyr::tibble(
    id = "ui_2dlrds",
    english = "RDS File",
    french = "Fichier RDS"
  ),
  dplyr::tibble(
    id = "ui_2dlplot",
    english = "PNG File",
    french = "Fichier PNG"
  ),
  dplyr::tibble(
    id = "ui_2dlcsv",
    english = "CSV File",
    french = "Fichier CSV"
  ),
  dplyr::tibble(
    id = "ui_2dlxlsx",
    english = "XLSX File",
    french = "Fichier XLSX"
  ),
  dplyr::tibble(
    id = "ui_3est",
    english = "Estimate hazard concentration",
    french = "Estimation de la concentration présentant un risque"
  ),
  dplyr::tibble(
    id = "ui_3bshint",
    english = "10,000 bootstrap samples recommended",
    french = "10 000 simulations bootstrap recommandées"
  ),
  dplyr::tibble(
    id = "ui_3thresh",
    english = "Fraction affected",
    french = "Fraction affectée"
  ),
  dplyr::tibble(
    id = "ui_3threshlabel",
    english = "Required estimate",
    french = "Estimation nécessaire"
  ),
  dplyr::tibble(
    id = "ui_3samples",
    english = "Bootstrap samples",
    french = "Simulations bootstrap"
  ),
  dplyr::tibble(
    id = "ui_3label",
    english = "Label by",
    french = "Organiser les étiquettes par"
  ),
  dplyr::tibble(
    id = "ui_3colour",
    english = "Colour by",
    french = "Organiser les couleurs par"
  ),
  dplyr::tibble(
    id = "ui_3symbol",
    english = "Symbol by",
    french = "Organiser les symboles par"
  ),
  dplyr::tibble(
    id = "ui_3plotopts",
    english = "Plot formatting options",
    french = "Options de formatage du graphique"
  ),
  dplyr::tibble(
    id = "ui_3pngopts",
    english = "PNG file formatting options",
    french = "Options de formatage du fichier PNG"
  ),
  dplyr::tibble(
    id = "ui_3model",
    english = "Plot model average and estimate hazard concentration",
    french = "Représentation de l'inférence multimodèle et estimation de la concentration présentant un risque"
  ),
  dplyr::tibble(
    id = "ui_3help",
    english = "Click 'Get CL' to calculate the upper and lower confidence limits (CL) for the estimate.",
    french = "Cliquer 'Obtenir bornes' pour obtenir les bornes inférieures et supérieures de l'intervalle de confiance  pour l’estimation."
  ),
  dplyr::tibble(
    id = "ui_3hc",
    english = "The model averaged estimate of the concentration that affects",
    french = "L'estimation par inférence multimodèle de la concentration affectant"
  ),
  dplyr::tibble(
    id = "ui_3hc2",
    english = "% of species is",
    french = "% des espèces est de"
  ),
  dplyr::tibble(
    id = "ui_3perc",
    english = "The model averaged estimate of the fraction affected by a concentration of",
    french = "L'estimation par inférence multimodèle du seuil pour une concentration de"
  ),
  dplyr::tibble(
    id = "ui_3perc2",
    english = "is",
    french = "est de"
  ),
  dplyr::tibble(
    id = "ui_3perc3",
    english = "% of species",
    french = "% des espèces"
  ),
  dplyr::tibble(
    id = "ui_3cl",
    english = "Get confidence limits",
    french = "Obtenir les bornes de l'intervalle de confiance"
  ),
  dplyr::tibble(
    id = "ui_3cl2",
    english = "Confidence limits",
    french = "Bornes de l'intervalle de confiance"
  ),
  dplyr::tibble(
    id = "ui_3pal",
    english = "Colour palette",
    french = "Éventail de couleur"
  ),
  dplyr::tibble(
    id = "ui_3xlab",
    english = "X-axis label",
    french = "Titre de l’axe des X"
  ),
  dplyr::tibble(
    id = "ui_3ylab",
    english = "Y-axis label",
    french = "Titre de l’axe des Y"
  ),
  dplyr::tibble(
    id = "ui_3title",
    english = "Plot title",
    french = "Titre du graphique"
  ),
  dplyr::tibble(
    id = "ui_3legend",
    english = "Colour legend title",
    french = "Titre de la légende des couleurs"
  ),
  dplyr::tibble(
    id = "ui_3shape",
    english = "Shape legend title",
    french = "Titre de la légende des symboles"
  ),
  dplyr::tibble(
    id = "ui_3width",
    english = "Width",
    french = "Largeur"
  ),
  dplyr::tibble(
    id = "ui_3height",
    english = "Height",
    french = "Hauteur"
  ),
  dplyr::tibble(
    id = "ui_3dpi",
    english = "Dpi (resolution)",
    french = "Dpi (résolution)"
  ),
  dplyr::tibble(
    id = "ui_3clbutton",
    english = "Get CL",
    french = "Obtenir bornes"
  ),
  dplyr::tibble(
    id = "ui_3cldesc1",
    english = "The selected % threshold to estimate hazard concentration is",
    french = "Le seuil (%) sélectionné pour estimer la concentration présentant un risque est de"
  ),
  dplyr::tibble(
    id = "ui_3cldesc11",
    english = "The selected hazard concentration to estimate % threshold is",
    french = "La concentration présentant un risque sélectionnée pour estimer le seuil (%) est de"
  ),
  dplyr::tibble(
    id = "ui_3cldesc2",
    english = "and the number of bootstrap samples is",
    french = "et le nombre de simulations bootstrap est de"
  ),
  dplyr::tibble(
    id = "ui_3cldesc3",
    english = "It will take around",
    french = "Cela prendra environ"
  ),
  dplyr::tibble(
    id = "ui_3cldesc4",
    english = "to generate confidence limits.",
    french = "pour générer les bornes de l'intervalle de confiance."
  ),
  dplyr::tibble(
    id = "ui_4help",
    english = "Copy and paste code below to reproduce results. Code is added as functions are executed within the app. (e.g., code for generating confidence limits will appear after 'Get CL' is clicked.)",
    french = "Copier et coller le code R ci-dessous pour reproduire les résultats. Le code est ajouté après chaque exécution de fonctions dans l’application (par exemple : le code qui génère l’estimation des intervalles de confiance apparaitra après que 'Obtenir bornes' soit cliqué)."
  ),
  dplyr::tibble(
    id = "ui_getreport",
    english = "Get Report",
    french = "Obtenir Rapport"
  ),
  dplyr::tibble(
    id = "ui_prevreport",
    english = "Preview report",
    french = "Prévisualiser le rapport"
  ),
  dplyr::tibble(
    id = "ui_draft",
    english = "This is a draft and may change at some point in the future.<br><br>",
    french = "Cette version est une ébauche et elle pourrait être modifiée en tout temps.<br><br>"
  ),
  dplyr::tibble(
    id = "ui_about",
    english = "This webpage fits species sensitivity distributions to concentration data. The user is able to select more than one distribution and plot the individual fits. <br/><br/>The columns in the goodness of fit table are the distribution (dist), the Anderson-Darling statistic (ad), the Kolmogorov-Smirnov statistic (ks), the Cramer-von Mises statistic (cvm), Akaike's Information Criterion (aic), Akaike's Information Criterion corrected for sample size (aicc), Bayesian Information Criterion (bic), the AICc difference (delta) and the AICc based Akaike weight (weight). The prediction is the model averaged (using aicc) estimate of the fit. The percent hazard concentration is the concentration of the chemical which is predicted to affect that percent of the species tested.<br/><br/>To cite package ssdtools in publications use:<br/>Thorley, J. and Schwarz C., (2018). ssdtools: An R package to fit Species Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082<br/><br/>To cite the web app use:<br/>Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. https://bcgov-env.shinyapps.io/ssdtools/",
    french = "Cette page Web ajuste les fonctions de distribution de sensibilité des espèces aux données de concentration. L'utilisateur peut sélectionner plus d'une distribution et représenter individuellement chaque courbe d'ajustement dans un graphique. <br/><br/> Les colonnes du tableau de l'évaluation de la qualité de l’ajustement des courbes de distributiont sont la distribution (dist), la statistique d’Anderson-Darling (ad), la statistique de Kolmogorov-Smirnov (ks), la statistique de Cramer-von-Mises (cmv), le critère d’information Akaike (aic), le critère d’information Akaike corrigé pour la taille de l’échantillon (aicc), le critère d’information Bayésien (bic), la différence entre AICc (delta) et la pondération des critères d'information AICc (coefficient de pondération). L’estimation de la fonction de distribution finale est basée sur l’inférence multimodèle (à partir de l’AICc). La concentration présentant un risque est la concentration estimée d’une substance affectant un centile (seuil) sélectionné de l’ensemble des espèces.<br/><br/>Pour citer l’application R ‘ssdtools’:<br/>Thorley, J. and Schwarz C., (2018). ssdtools: An R package to fit Species Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082<br/><br/>Pour citer l’application web :<br/>Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. https://bcgov-env.shinyapps.io/ssdtools/"
  ),
  dplyr::tibble(
    id = "ui_hintdata",
    english = "You have not added a dataset.",
    french = "Aucun ensemble de données n’a été ajouté."
  ),
  dplyr::tibble(
    id = "ui_hintconc0",
    english = "Concentration must not be 0.",
    french = "La concentration ne doit pas être zéro."
  ),
  dplyr::tibble(
    id = "ui_hintconcmiss",
    english = "Concentration must not be missing.",
    french = "La concentration ne doit être manquante."
  ),
  dplyr::tibble(
    id = "ui_hintnum",
    english = "Concentration column must contain number.",
    french = "La colonne 'concentration' doit contenir des nombres."
  ),
  dplyr::tibble(
    id = "ui_hintmiss",
    english = "Concentration values must not be missing.",
    french = "Aucune valeur de concentration ne doit être manquante."
  ),
  dplyr::tibble(
    id = "ui_hintpos",
    english = "Concentration values must be positive.",
    french = "Les valeurs de concentration doivent être des  positives.  "
  ),
  dplyr::tibble(
    id = "ui_hintfin",
    english = "Concentration values must be finite.",
    french = "Les valeurs de concentration ne peuvent pas être censurées."
  ),
  dplyr::tibble(
    id = "ui_hintident",
    english = "Concentration values must not all be identical.",
    french = "Les valeur de concentration ne doivent pas être toutes identiques."
  ),
  dplyr::tibble(
    id = "ui_hint6",
    english = "There must be at least 6 concentration values.",
    french = "Six valeurs de concentration sont minimalement requises. "
  ),
  dplyr::tibble(
    id = "ui_hintdist",
    english = "At least one distribution must be selected.",
    french = "Au moins une distribution doit  être sélectionnée. "
  ),
  dplyr::tibble(
    id = "ui_hintpred",
    english = "You must select the 'Fit' tab before using the 'Predict' tab.",
    french = "Sélectionner l’onglet 'Ajustement' avant l’onglet 'Estimation'."
  ),
  dplyr::tibble(
    id = "ui_hintfit",
    english = "You have not successfully fit any distributions yet. Run the 'Fit' tab first.",
    french = "Aucune distribution n’a encore été ajustée avec succès. Exécuter d'abord l’onglet 'Ajustement'."
  ),
  dplyr::tibble(
    id = "ui_hintpredict",
    english = "You have not successfully generated predictions yet. Run the 'Predict' tab first.",
    french = "Aucune prédiction n'a encore été générée avec succès. Exécuter d'abord l'onglet 'Estimation'."
  ),
  dplyr::tibble(
    id = "ui_hintfail",
    english = "distribution(s) failed to fit. Run R code to get more information.",
    french = "échec dans l’ajustement des distributions. Exécuter le code R pour plus d’informations."
  ),
  dplyr::tibble(
    id = "ui_hintcolour",
    english = "Colour variable cannot be numeric.",
    french = "La colonne 'Colour' ne doit pas contenir de nombres."
  ),
  dplyr::tibble(
    id = "ui_hintsym",
    english = "Symbol variable cannot be numeric.",
    french = "La colonne 'Symboles' ne doit pas contenir de nombres."
  ),
  dplyr::tibble(
    id = "ui_copy",
    english = "Copy code",
    french = "Copier le code"
  )
,
  dplyr::tibble(
    id = "ui_3conc",
    english = "Concentration",
    french = "Concentration"
  ),
  dplyr::tibble(
    id = "ui_thresh_type",
    english = "Get estimate by",
    french = "Obtenir l’estimation par"
  ),
  dplyr::tibble(
    id = "ui_checkHc",
    english = "Plot Threshold/Concentration",
    french = "Graphique seuil/concentration"
  ),
  # dplyr::tibble(
  #   id = "ui_3hc",
  #   english = "The model averaged estimate of the concentration that affects {percent} % of species is {conc}",
  #   french = "Selon le modèle agrégé, la concentration affectant {percent} % des espèces est de {conc}"
  # ),
  # dplyr::tibble(
  #   id = "ui_3hc2",
  #   english = "The model averaged estimate of the fraction affected by a concentration of {conc} is {percent} % of species",
  #   french = "L'estimation par inférence multimodèle du seuil pour une concentration de {conc} est de {percent} % des espèces."
  # ),
  dplyr::tibble(
    id = "ui_1table1",
    english = "Table",
    french = "Table"
  ),
  dplyr::tibble(
    id = "ui_2rescale",
    english = "Rescale data prior to fitting",
    french = "Redimensionner les données avant l'ajustement"
  ),
  dplyr::tibble(
    id = "ui_xmax",
    english = "X-axis maximum",
    french = "Maximum de l'axe des X"
  ),
  dplyr::tibble(
    id = "ui_xmin",
    english = "X-axis minimum",
    french = "Minimum de l'axe des X"
  ),
  dplyr::tibble(
    id = "ui_xlog",
    english = "Log x-axis",
    french = "Log de l'axe X"
  ),
  dplyr::tibble(
    id = "ui_sizeLabel",
    english = "Label size",
    french = "Taille de l'étiquette"
  ),
  dplyr::tibble(
    id = "ui_size",
    english = "Text size",
    french = "Taille du texte"
  ),
  dplyr::tibble(
    id = "ui_adjustLabel",
    english = "Shift label",
    french = "Étiquette de changement"
  ),
  dplyr::tibble(
    id = "ui_xbreaks",
    english = "X-axis ticks",
    french = "Repères de l'axe des X"
  ),
  dplyr::tibble(
    id = "ui_2unit",
    english = "Select units",
    french = "Sélectionner les unités"
  ),
  dplyr::tibble(
    id = "ui_3byconc",
    english = "by concentration",
    french = "par concentration"
  ),
  dplyr::tibble(
    id = "ui_3affecting",
    english = "affecting % species",
    french = "affectant % des espèces"
  ),
  dplyr::tibble(
    id = "ui_3protecting",
    english = "protecting % species",
    french = "protégeant % des espèces"
  ),
  dplyr::tibble(
    id = "ui_4toxname",
    english = "Toxicant name",
    french = "Nom de la substance"
  ),
  dplyr::tibble(
    id = "ui_4download",
    english = "Download Report",
    french = "Télécharger le Rapport"
  ),
  dplyr::tibble(
    id = "ui_4pdf",
    english = "PDF file",
    french = "Fichier PDF"
  ),
  dplyr::tibble(
    id = "ui_4html",
    english = "HTML file",
    french = "Fichier HTML"
  ),
  dplyr::tibble(
    id = "ui_4rmd",
    english = "RMD file",
    french = "Fichier RMD"
  ),
  dplyr::tibble(
    id = "ui_4gentitle",
    english = "Generating report ...",
    french = "Génération du rapport en cours ... "
  ),
  dplyr::tibble(
    id = "ui_4genbody",
    english = "This may take a minute, depending on the number of bootstrap samples selected.",
    french = "Cela peut prendre une minute, en fonction du nombre d'échantillons bootstrap sélectionné."
  ),
  dplyr::tibble(
    id = "ui_1htconc",
    english = "Conc",
    french = "Conc"
  ),
  dplyr::tibble(
    id = "ui_1htspp",
    english = "Species",
    french = "Espèce"
  ),
  dplyr::tibble(
    id = "ui_1htgrp",
    english = "Group",
    french = "Groupe"
  ),
  dplyr::tibble(
    id = "ui_1htchm",
    english = "Chemical",
    french = "Produit Chimique"
  ),
  dplyr::tibble(
    id = "ui_1htunt",
    english = "Units",
    french = "Unités"
  ),
  dplyr::tibble(
    id = "ui_bcanz_file",
    english = "bcanz_report.Rmd",
    french = "bcanz_report_fr.Rmd"
  ),
  dplyr::tibble(
    id = "ui_bcanz_filename",
    english = "bcanz_report",
    french = "rapport_bcanz"
  ),
  dplyr::tibble(
    id = "ui_update_fit",
    english = "Update Fit",
    french = "Mettre à jour l'ajustement"
  ),
  dplyr::tibble(
    id = "ui_update_data",
    english = "Update",
    french = "Mettre à jour"
  )
)

chk::check_key(translations, "id")

boron.data <- ssddata::ccme_boron

pal <- RColorBrewer::brewer.pal.info
pals <- pal[which(pal$category == "qual"), ] %>% row.names()

default.dists <- ssdtools::ssd_dists_bcanz()
extra.dists <- setdiff(ssdtools::ssd_dists_all(), default.dists)

usethis::use_data(
  boron.data,
  translations,
  pals,
  default.dists,
  extra.dists,
  internal = TRUE,
  overwrite = TRUE
)
