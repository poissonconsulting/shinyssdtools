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

library(dplyr)

# All translations unified from CSV (previously split between Excel and manual tibbles)
translations <- dplyr::bind_rows(
  dplyr::tibble(
    id = "ui_navtitle",
    english = "Fit and Plot Species Sensitivity Distributions",
    french = "Ajustement et Représentation des courbes de Distribution de la Sensibilité des Espèces",
    spanish = "Ajuste y Gráfico de Distribuciones de Sensibilidad de Especies",
    japanese = "種の感受性分布の適合とプロット"
  ),
  dplyr::tibble(
    id = "ui_nav1",
    english = "1. Data",
    french = "1. Données",
    spanish = "1. Datos",
    japanese = "1. データ"
  ),
  dplyr::tibble(
    id = "ui_nav2",
    english = "2. Fit",
    french = "2. Ajustement",
    spanish = "2. Ajuste",
    japanese = "2. 適合"
  ),
  dplyr::tibble(
    id = "ui_nav3",
    english = "3. Predict",
    french = "3. Estimation",
    spanish = "3. Predicción",
    japanese = "3. 予測"
  ),
  dplyr::tibble(
    id = "ui_nav4",
    english = "4. Report",
    french = "4. Rapport",
    spanish = "4. Informe",
    japanese = "4. レポート"
  ),
  dplyr::tibble(
    id = "ui_nav5",
    english = "R Code",
    french = "Code R",
    spanish = "Código R",
    japanese = "Rコード"
  ),
  dplyr::tibble(
    id = "ui_navanalyse",
    english = "Analyse",
    french = "Analyser",
    spanish = "Analizar",
    japanese = "分析"
  ),
  dplyr::tibble(
    id = "ui_navabout",
    english = "About",
    french = "À propos",
    spanish = "Acerca de",
    japanese = "について"
  ),
  dplyr::tibble(
    id = "ui_navguide",
    english = "User Guide",
    french = "Guide de l'utilisateur",
    spanish = "Guía del Usuario",
    japanese = "ユーザーガイド"
  ),
  dplyr::tibble(
    id = "ui_navlang",
    english = "Language",
    french = "Langue",
    spanish = "Idioma",
    japanese = "言語"
  ),
  dplyr::tibble(
    id = "ui_tabdata",
    english = "Provide data",
    french = "Fournir les données",
    spanish = "Proporcionar datos",
    japanese = "データを提供"
  ),
  dplyr::tibble(
    id = "ui_tabfit",
    english = "Fit distributions",
    french = "Ajustement des distributions",
    spanish = "Ajustar distribuciones",
    japanese = "分布を適合"
  ),
  dplyr::tibble(
    id = "ui_tabpredict",
    english = "Estimate hazard concentration",
    french = "Estimation de la concentration présentant un risque",
    spanish = "Estimar concentración peligrosa",
    japanese = "ハザード濃度を推定"
  ),
  dplyr::tibble(
    id = "ui_tabreport",
    english = "Get BCANZ report",
    french = "Obtenir le BCANZ rapport",
    spanish = "Obtener informe BCANZ",
    japanese = "BCANZレポートを取得"
  ),
  dplyr::tibble(
    id = "ui_tabcode",
    english = "Get R code",
    french = "Obtenir le code R",
    spanish = "Obtener código R",
    japanese = "Rコードを取得"
  ),
  dplyr::tibble(
    id = "ui_1choose",
    english = "Choose one of the following options:",
    french = "Choisir l’une des options suivantes :",
    spanish = "Elija una de las siguientes opciones:",
    japanese = "次のオプションのいずれかを選択してください："
  ),
  dplyr::tibble(
    id = "ui_1data",
    english = "1. Use",
    french = "1. Utiliser",
    spanish = "1. Usar",
    japanese = "1. 使用"
  ),
  dplyr::tibble(
    id = "ui_1data2",
    english = "boron dataset",
    french = "l’ensemble de données pour le bore",
    spanish = "conjunto de datos de boro",
    japanese = "ホウ素データセット"
  ),
  dplyr::tibble(
    id = "ui_1title",
    english = "1. Data",
    french = "1. Données",
    spanish = "1. Datos",
    japanese = "1. データ"
  ),
  dplyr::tibble(
    id = "ui_1datahelp",
    english = "1. This can be used to demo the app or view a dataset that 'works'.",
    french = "1. Ces données peuvent être téléchargées pour visualiser un ensemble 'fonctionnel' de données.",
    spanish = "1. Esto se puede usar para demostrar la aplicación o ver un conjunto de datos que 'funciona'.",
    japanese = "1. これはアプリのデモやデータセットの表示に使用できます。"
  ),
  dplyr::tibble(
    id = "ui_1csv",
    english = "2. Upload CSV file",
    french = "2. Télécharger un fichier CSV",
    spanish = "2. Cargar archivo CSV",
    japanese = "2. CSVファイルをアップロード"
  ),
  dplyr::tibble(
    id = "ui_1csvlabel",
    english = "Upload your data",
    french = "Télécharger les données",
    spanish = "Cargar sus datos",
    japanese = "データをアップロード"
  ),
  dplyr::tibble(
    id = "ui_1table",
    english = "3. Fill out table below:",
    french = "3. Remplir le tableau ci-dessous:",
    spanish = "3. Completar la tabla a continuación:",
    japanese = "3. 以下の表に記入してください："
  ),

  dplyr::tibble(
    id = "ui_1note",
    english = "Note: the app is designed to handle one chemical at a time. Each species should not have more than one concentration value.",
    french = "Note : L’application est faite pour travailler un contaminant à la fois. Il n’est pas possible d’entrer plus d’une concentration par espèce.",
    spanish = "Nota: la aplicación está diseñada para manejar un químico a la vez. Cada especie no debe tener más de un valor de concentración.",
    japanese = "注：このアプリは一度に1つの化学物質を処理するように設計されています。各種に複数の濃度値を設定しないでください。"
  ),
  dplyr::tibble(
    id = "ui_1preview",
    english = "Preview chosen dataset",
    french = "Prévisualiser l'ensemble de données",
    spanish = "Vista previa del conjunto de datos elegido",
    japanese = "選択したデータセットをプレビュー"
  ),
  dplyr::tibble(
    id = "ui_2title",
    english = "2. Fit",
    french = "2. Ajustement",
    spanish = "2. Ajuste",
    japanese = "2. 適合"
  ),
  dplyr::tibble(
    id = "ui_2conc",
    english = "Select column with concentration values",
    french = "Sélectionner la colonne avec les valeurs de concentration",
    spanish = "Seleccionar columna con valores de concentración",
    japanese = "濃度値の列を選択"
  ),
  dplyr::tibble(
    id = "ui_2dist",
    english = "Select distributions to fit",
    french = "Sélectionner les distributions pour l’ajustement",
    spanish = "Seleccionar distribuciones para ajustar",
    japanese = "適合する分布を選択"
  ),
  dplyr::tibble(
    id = "ui_2png",
    english = "PNG file formatting options",
    french = "Options de formatage du fichier PNG",
    spanish = "Opciones de formato de archivo PNG",
    japanese = "PNGファイル形式のオプション"
  ),
  dplyr::tibble(
    id = "ui_2plot",
    english = "Plot fitted distributions",
    french = "Représentation des courbes de distribution ajustées",
    spanish = "Gráfico de distribuciones ajustadas",
    japanese = "適合した分布をプロット"
  ),
  dplyr::tibble(
    id = "ui_2table",
    english = "Goodness of Fit table",
    french = "Tableau de l'évaluation de la qualité de l’ajustement des courbes de distribution",
    spanish = "Tabla de Bondad de Ajuste",
    japanese = "適合度テーブル"
  ),
  dplyr::tibble(
    id = "ui_2weight",
    english = "weight",
    french = "coefficient de pondération",
    spanish = "peso",
    japanese = "重み"
  ),
  dplyr::tibble(
    id = "ui_2ploty",
    english = "Species affected (%)",
    french = "Pourcentage d’espèces affectées",
    spanish = "Especies afectadas (%)",
    japanese = "影響を受けた種（％）"
  ),
  dplyr::tibble(
    id = "ui_2download",
    english = "Download",
    french = "Télécharger",
    spanish = "Descargar",
    japanese = "ダウンロード"
  ),
  dplyr::tibble(
    id = "ui_2height",
    english = "Height",
    french = "Hauteur",
    spanish = "Altura",
    japanese = "高さ"
  ),
  dplyr::tibble(
    id = "ui_2width",
    english = "Width",
    french = "Largeur",
    spanish = "Ancho",
    japanese = "幅"
  ),
  dplyr::tibble(
    id = "ui_2dpi",
    english = "Dpi",
    french = "Dpi",
    spanish = "Dpi",
    japanese = "解像度"
  ),
  dplyr::tibble(
    id = "ui_2dlpdf",
    english = "PDF File",
    french = "Fichier PDF",
    spanish = "Archivo PDF",
    japanese = "PDFファイル"
  ),
  dplyr::tibble(
    id = "ui_2dlhtml",
    english = "HTML File",
    french = "Fichier HTML",
    spanish = "Archivo HTML",
    japanese = "HTMLファイル"
  ),
  dplyr::tibble(
    id = "ui_2dlrds",
    english = "RDS File",
    french = "Fichier RDS",
    spanish = "Archivo RDS",
    japanese = "RDSファイル"
  ),
  dplyr::tibble(
    id = "ui_2dlplot",
    english = "PNG File",
    french = "Fichier PNG",
    spanish = "Archivo PNG",
    japanese = "PNGファイル"
  ),
  dplyr::tibble(
    id = "ui_2dlcsv",
    english = "CSV File",
    french = "Fichier CSV",
    spanish = "Archivo CSV",
    japanese = "CSVファイル"
  ),
  dplyr::tibble(
    id = "ui_2dlxlsx",
    english = "XLSX File",
    french = "Fichier XLSX",
    spanish = "Archivo XLSX",
    japanese = "XLSXファイル"
  ),
  dplyr::tibble(
    id = "ui_3tabtitle",
    english = "3. Predict",
    french = "3. Prédiction",
    spanish = "3. Predicción",
    japanese = "3. 予測"
  ),
  dplyr::tibble(
    id = "ui_3est",
    english = "Estimate hazard concentration",
    french = "Estimation de la concentration présentant un risque",
    spanish = "Estimar concentración peligrosa",
    japanese = "ハザード濃度を推定"
  ),
  dplyr::tibble(
    id = "ui_3bshint",
    english = "10,000 bootstrap samples recommended",
    french = "10 000 simulations bootstrap recommandées",
    spanish = "Se recomiendan 10.000 muestras bootstrap",
    japanese = "10,000ブートストラップサンプルを推奨"
  ),
  dplyr::tibble(
    id = "ui_3thresh",
    english = "Fraction affected",
    french = "Fraction affectée",
    spanish = "Fracción afectada",
    japanese = "影響を受けた割合"
  ),
  dplyr::tibble(
    id = "ui_3threshlabel",
    english = "Required estimate",
    french = "Estimation nécessaire",
    spanish = "Estimación requerida",
    japanese = "必要な推定値"
  ),
  dplyr::tibble(
    id = "ui_3samples",
    english = "Bootstrap samples",
    french = "Simulations bootstrap",
    spanish = "Muestras bootstrap",
    japanese = "ブートストラップサンプル"
  ),
  dplyr::tibble(
    id = "ui_3label",
    english = "Label by",
    french = "Organiser les étiquettes par",
    spanish = "Etiquetar por",
    japanese = "ラベル別"
  ),
  dplyr::tibble(
    id = "ui_3colour",
    english = "Colour by",
    french = "Organiser les couleurs par",
    spanish = "Colorear por",
    japanese = "色別"
  ),
  dplyr::tibble(
    id = "ui_3symbol",
    english = "Symbol by",
    french = "Organiser les symboles par",
    spanish = "Símbolo por",
    japanese = "シンボル別"
  ),
  dplyr::tibble(
    id = "ui_3plotopts",
    english = "Plot formatting options",
    french = "Options de formatage du graphique",
    spanish = "Opciones de formato de gráfico",
    japanese = "プロット形式のオプション"
  ),
  dplyr::tibble(
    id = "ui_3pngopts",
    english = "PNG file formatting options",
    french = "Options de formatage du fichier PNG",
    spanish = "Opciones de formato de archivo PNG",
    japanese = "PNGファイル形式のオプション"
  ),
  dplyr::tibble(
    id = "ui_3model",
    english = "Plot model average and estimate hazard concentration",
    french = "Représentation de l'inférence multimodèle et estimation de la concentration présentant un risque",
    spanish = "Gráfico de promedio de modelo y estimación de concentración peligrosa",
    japanese = "モデル平均のプロットとハザード濃度の推定"
  ),
  dplyr::tibble(
    id = "ui_3help",
    english = "Click 'Get CL' to calculate the upper and lower confidence limits (CL) for the estimate.",
    french = "Cliquer 'Obtenir bornes' pour obtenir les bornes inférieures et supérieures de l'intervalle de confiance  pour l’estimation.",
    spanish = "Haga clic en 'Obtener LC' para calcular los límites de confianza superior e inferior (LC) para la estimación.",
    japanese = "「CLを取得」をクリックして、推定値の上限と下限の信頼限界（CL）を計算します。"
  ),
  dplyr::tibble(
    id = "ui_3hc",
    english = "The model averaged estimate of the concentration that affects",
    french = "L'estimation par inférence multimodèle de la concentration affectant",
    spanish = "La estimación promediada del modelo de la concentración que afecta",
    japanese = "影響を与える濃度のモデル平均推定値"
  ),
  dplyr::tibble(
    id = "ui_3hc2",
    english = "% of species is",
    french = "% des espèces est de",
    spanish = "% de las especies es",
    japanese = "％の種は"
  ),
  dplyr::tibble(
    id = "ui_3perc",
    english = "The model averaged estimate of the fraction affected by a concentration of",
    french = "L'estimation par inférence multimodèle du seuil pour une concentration de",
    spanish = "La estimación promediada del modelo de la fracción afectada por una concentración de",
    japanese = "濃度によって影響を受ける割合のモデル平均推定値"
  ),
  dplyr::tibble(
    id = "ui_3perc2",
    english = "is",
    french = "est de",
    spanish = "es",
    japanese = "は"
  ),
  dplyr::tibble(
    id = "ui_3perc3",
    english = "% of species",
    french = "% des espèces",
    spanish = "% de las especies",
    japanese = "％の種"
  ),
  dplyr::tibble(
    id = "ui_3cl",
    english = "Get confidence limits",
    french = "Obtenir les bornes de l'intervalle de confiance",
    spanish = "Obtener límites de confianza",
    japanese = "信頼限界を取得"
  ),
  dplyr::tibble(
    id = "ui_3cl2",
    english = "Confidence limits",
    french = "Bornes de l'intervalle de confiance",
    spanish = "Límites de confianza",
    japanese = "信頼限界"
  ),
  dplyr::tibble(
    id = "ui_3includeci",
    english = "Include on model average plot",
    french = "Inclure sur le graphique de la moyenne du modèle",
    spanish = "Incluir en gráfico de promedio del modelo",
    japanese = "モデル平均プロットに含める"
  ),
  dplyr::tibble(
    id = "ui_3ribbonstyle",
    english = "Model averaged estimate and CL style",
    french = "Style de l'estimation moyenne du modèle et bornes",
    spanish = "Estilo de estimación promediada del modelo y LC",
    japanese = "モデル平均推定値とCLスタイル"
  ),
  dplyr::tibble(
    id = "ui_3ribbonblack",
    english = "Ribbon",
    french = "Ruban",
    spanish = "Cinta",
    japanese = "リボン"
  ),
  dplyr::tibble(
    id = "ui_3ribbonlines",
    english = "Lines",
    french = "Lignes",
    spanish = "Líneas",
    japanese = "線"
  ),
  dplyr::tibble(
    id = "ui_3pal",
    english = "Colour palette",
    french = "Éventail de couleur",
    spanish = "Paleta de colores",
    japanese = "カラーパレット"
  ),
  dplyr::tibble(
    id = "ui_3xlab",
    english = "X-axis label",
    french = "Titre de l’axe des X",
    spanish = "Etiqueta del eje X",
    japanese = "X軸ラベル"
  ),
  dplyr::tibble(
    id = "ui_3ylab",
    english = "Y-axis label",
    french = "Titre de l’axe des Y",
    spanish = "Etiqueta del eje Y",
    japanese = "Y軸ラベル"
  ),
  dplyr::tibble(
    id = "ui_3title",
    english = "Plot title",
    french = "Titre du graphique",
    spanish = "Título del gráfico",
    japanese = "プロットタイトル"
  ),
  dplyr::tibble(
    id = "ui_3legend",
    english = "Colour legend title",
    french = "Titre de la légende des couleurs",
    spanish = "Título de leyenda de color",
    japanese = "色の凡例タイトル"
  ),
  dplyr::tibble(
    id = "ui_3shape",
    english = "Shape legend title",
    french = "Titre de la légende des symboles",
    spanish = "Título de leyenda de forma",
    japanese = "形状の凡例タイトル"
  ),
  dplyr::tibble(
    id = "ui_3width",
    english = "Width",
    french = "Largeur",
    spanish = "Ancho",
    japanese = "幅"
  ),
  dplyr::tibble(
    id = "ui_3height",
    english = "Height",
    french = "Hauteur",
    spanish = "Altura",
    japanese = "高さ"
  ),
  dplyr::tibble(
    id = "ui_3dpi",
    english = "Dpi (resolution)",
    french = "Dpi (résolution)",
    spanish = "Dpi (resolución)",
    japanese = "解像度"
  ),
  dplyr::tibble(
    id = "ui_3clbutton",
    english = "Get CL",
    french = "Obtenir bornes",
    spanish = "Obtener LC",
    japanese = "CLを取得"
  ),
  dplyr::tibble(
    id = "ui_3cldesc1",
    english = "The selected % threshold to estimate hazard concentration is",
    french = "Le seuil (%) sélectionné pour estimer la concentration présentant un risque est de",
    spanish = "El umbral de % seleccionado para estimar la concentración peligrosa es",
    japanese = "ハザード濃度を推定するために選択された％閾値は"
  ),
  dplyr::tibble(
    id = "ui_3cldesc11",
    english = "The selected hazard concentration to estimate % threshold is",
    french = "La concentration présentant un risque sélectionnée pour estimer le seuil (%) est de",
    spanish = "La concentración peligrosa seleccionada para estimar el umbral de % es",
    japanese = "％閾値を推定するために選択されたハザード濃度は"
  ),
  dplyr::tibble(
    id = "ui_3cldesc2",
    english = "and the number of bootstrap samples is",
    french = "et le nombre de simulations bootstrap est de",
    spanish = "y el número de muestras bootstrap es",
    japanese = "そしてブートストラップサンプル数は"
  ),
  dplyr::tibble(
    id = "ui_3cldesc3",
    english = "It will take around",
    french = "Cela prendra environ",
    spanish = "Tomará alrededor de",
    japanese = "約"
  ),
  dplyr::tibble(
    id = "ui_3cldesc4",
    english = "to generate confidence limits.",
    french = "pour générer les bornes de l'intervalle de confiance.",
    spanish = "para generar límites de confianza.",
    japanese = "かかります（信頼限界の生成）。"
  ),
  dplyr::tibble(
    id = "ui_4help",
    english = "Copy and paste code below to reproduce results. Code is added as functions are executed within the app. (e.g., code for generating confidence limits will appear after 'Get CL' is clicked.)",
    french = "Copier et coller le code R ci-dessous pour reproduire les résultats. Le code est ajouté après chaque exécution de fonctions dans l’application (par exemple : le code qui génère l’estimation des intervalles de confiance apparaitra après que 'Obtenir bornes' soit cliqué).",
    spanish = "Copie y pegue el código a continuación para reproducir los resultados. El código se agrega a medida que se ejecutan las funciones dentro de la aplicación (por ejemplo, el código para generar límites de confianza aparecerá después de hacer clic en 'Obtener LC').",
    japanese = "結果を再現するには、以下のコードをコピーして貼り付けてください。コードは、アプリ内で関数が実行されると追加されます（例：「CLを取得」をクリックすると、信頼限界を生成するコードが表示されます）。"
  ),
  dplyr::tibble(
    id = "ui_getreport",
    english = "Get Report",
    french = "Obtenir Rapport",
    spanish = "Obtener Informe",
    japanese = "レポートを取得"
  ),
  dplyr::tibble(
    id = "ui_prevreport",
    english = "Preview report",
    french = "Prévisualiser le rapport",
    spanish = "Vista previa del informe",
    japanese = "レポートをプレビュー"
  ),
  dplyr::tibble(
    id = "ui_draft",
    english = "This is a draft and may change at some point in the future.<br><br>",
    french = "Cette version est une ébauche et elle pourrait être modifiée en tout temps.<br><br>",
    spanish = "Este es un borrador y puede cambiar en algún momento en el futuro.<br><br>",
    japanese = "これはドラフトであり、将来変更される可能性があります。<br><br>"
  ),
  dplyr::tibble(
    id = "ui_about",
    english = "This webpage fits species sensitivity distributions to concentration data. The user is able to select more than one distribution and plot the individual fits. <br/><br/>The columns in the goodness of fit table are the distribution (dist), the Anderson-Darling statistic (ad), the Kolmogorov-Smirnov statistic (ks), the Cramer-von Mises statistic (cvm), Akaike's Information Criterion (aic), Akaike's Information Criterion corrected for sample size (aicc), Bayesian Information Criterion (bic), the AICc difference (delta) and the AICc based Akaike weight (weight). The prediction is the model averaged (using aicc) estimate of the fit. The percent hazard concentration is the concentration of the chemical which is predicted to affect that percent of the species tested.<br/><br/>To cite package ssdtools in publications use:<br/>Thorley, J. and Schwarz C., (2018). ssdtools: An R package to fit Species Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082<br/><br/>To cite the web app use:<br/>Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. https://bcgov-env.shinyapps.io/ssdtools/",
    french = "Cette page Web ajuste les fonctions de distribution de sensibilité des espèces aux données de concentration. L'utilisateur peut sélectionner plus d'une distribution et représenter individuellement chaque courbe d'ajustement dans un graphique. <br/><br/> Les colonnes du tableau de l'évaluation de la qualité de l’ajustement des courbes de distributiont sont la distribution (dist), la statistique d’Anderson-Darling (ad), la statistique de Kolmogorov-Smirnov (ks), la statistique de Cramer-von-Mises (cmv), le critère d’information Akaike (aic), le critère d’information Akaike corrigé pour la taille de l’échantillon (aicc), le critère d’information Bayésien (bic), la différence entre AICc (delta) et la pondération des critères d'information AICc (coefficient de pondération). L’estimation de la fonction de distribution finale est basée sur l’inférence multimodèle (à partir de l’AICc). La concentration présentant un risque est la concentration estimée d’une substance affectant un centile (seuil) sélectionné de l’ensemble des espèces.<br/><br/>Pour citer l’application R ‘ssdtools’:<br/>Thorley, J. and Schwarz C., (2018). ssdtools: An R package to fit Species Sensitivity Distributions. Journal of Open Source Software, 3(31), 1082. https://doi.org/10.21105/joss.01082<br/><br/>Pour citer l’application web :<br/>Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. https://bcgov-env.shinyapps.io/ssdtools/"
  ),
  dplyr::tibble(
    id = "ui_hintdata",
    english = "You have not added a dataset.",
    french = "Aucun ensemble de données n’a été ajouté.",
    spanish = "No ha agregado un conjunto de datos.",
    japanese = "データセットが追加されていません。"
  ),
  dplyr::tibble(
    id = "ui_hintconc0",
    english = "Concentration must not be 0.",
    french = "La concentration ne doit pas être zéro.",
    spanish = "La concentración no debe ser 0.",
    japanese = "濃度は0であってはなりません。"
  ),
  dplyr::tibble(
    id = "ui_hintconcmiss",
    english = "Concentration must not be missing.",
    french = "La concentration ne doit être manquante.",
    spanish = "La concentración no debe faltar.",
    japanese = "濃度が欠落していてはいけません。"
  ),
  dplyr::tibble(
    id = "ui_hintnum",
    english = "Concentration column must contain number.",
    french = "La colonne 'concentration' doit contenir des nombres.",
    spanish = "La columna de concentración debe contener números.",
    japanese = "濃度列には数値が含まれている必要があります。"
  ),
  dplyr::tibble(
    id = "ui_hintmiss",
    english = "Concentration values must not be missing.",
    french = "Aucune valeur de concentration ne doit être manquante.",
    spanish = "Los valores de concentración no deben faltar.",
    japanese = "濃度値が欠落していてはいけません。"
  ),
  dplyr::tibble(
    id = "ui_hintpos",
    english = "Concentration values must be positive.",
    french = "Les valeurs de concentration doivent être des  positives.  ",
    spanish = "Los valores de concentración deben ser positivos.",
    japanese = "濃度値は正の値でなければなりません。"
  ),
  dplyr::tibble(
    id = "ui_hintfin",
    english = "Concentration values must be finite.",
    french = "Les valeurs de concentration ne peuvent pas être censurées.",
    spanish = "Los valores de concentración deben ser finitos.",
    japanese = "濃度値は有限でなければなりません。"
  ),
  dplyr::tibble(
    id = "ui_hintident",
    english = "Concentration values must not all be identical.",
    french = "Les valeur de concentration ne doivent pas être toutes identiques.",
    spanish = "Los valores de concentración no deben ser todos idénticos.",
    japanese = "濃度値がすべて同じであってはいけません。"
  ),
  dplyr::tibble(
    id = "ui_hint6",
    english = "There must be at least 6 concentration values.",
    french = "Six valeurs de concentration sont minimalement requises. ",
    spanish = "Debe haber al menos 6 valores de concentración.",
    japanese = "少なくとも6つの濃度値が必要です。"
  ),
  dplyr::tibble(
    id = "ui_hintdist",
    english = "At least one distribution must be selected.",
    french = "Au moins une distribution doit  être sélectionnée. ",
    spanish = "Se debe seleccionar al menos una distribución.",
    japanese = "少なくとも1つの分布を選択する必要があります。"
  ),
  dplyr::tibble(
    id = "ui_hintpred",
    english = "You must select the 'Fit' tab before using the 'Predict' tab.",
    french = "Sélectionner l’onglet 'Ajustement' avant l’onglet 'Estimation'.",
    spanish = "Debe seleccionar la pestaña 'Ajuste' antes de usar la pestaña 'Predicción'.",
    japanese = "「予測」タブを使用する前に「適合」タブを選択する必要があります。"
  ),
  dplyr::tibble(
    id = "ui_hintfit",
    english = "You have not successfully fit any distributions yet. Run the 'Fit' tab first.",
    french = "Aucune distribution n’a encore été ajustée avec succès. Exécuter d'abord l’onglet 'Ajustement'.",
    spanish = "Aún no ha ajustado ninguna distribución con éxito. Ejecute primero la pestaña 'Ajuste'.",
    japanese = "まだ分布の適合に成功していません。最初に「適合」タブを実行してください。"
  ),
  dplyr::tibble(
    id = "ui_hintpredict",
    english = "You have not successfully generated predictions yet. Run the 'Predict' tab first.",
    french = "Aucune prédiction n'a encore été générée avec succès. Exécuter d'abord l'onglet 'Estimation'.",
    spanish = "Aún no ha generado predicciones con éxito. Ejecute primero la pestaña 'Predicción'.",
    japanese = "まだ予測の生成に成功していません。最初に「予測」タブを実行してください。"
  ),
  dplyr::tibble(
    id = "ui_hintfail",
    english = "distribution(s) failed to fit. Run R code to get more information.",
    french = "échec dans l’ajustement des distributions. Exécuter le code R pour plus d’informations.",
    spanish = "distribución(es) no se pudieron ajustar. Ejecute el código R para obtener más información.",
    japanese = "分布の適合に失敗しました。詳細情報はRコードを実行してください。"
  ),
  dplyr::tibble(
    id = "ui_hintcolour",
    english = "Colour variable cannot be numeric.",
    french = "La colonne 'Colour' ne doit pas contenir de nombres.",
    spanish = "La variable de color no puede ser numérica.",
    japanese = "色変数は数値であってはいけません。"
  ),
  dplyr::tibble(
    id = "ui_hintsym",
    english = "Symbol variable cannot be numeric.",
    french = "La colonne 'Symboles' ne doit pas contenir de nombres.",
    spanish = "La variable de símbolo no puede ser numérica.",
    japanese = "シンボル変数は数値であってはいけません。"
  ),
  dplyr::tibble(
    id = "ui_hintthresh",
    english = "Affecting % species must not be missing.",
    french = "Le % d'espèces affectées ne doit pas être manquant.",
    spanish = "El % de especies afectadas no debe faltar.",
    japanese = "影響を受ける種の％が欠落していてはいけません。"
  ),
  dplyr::tibble(
    id = "ui_hintthreshpc",
    english = "Protecting % species must not be missing.",
    french = "Le % d'espèces protégées ne doit pas être manquant.",
    spanish = "El % de especies protegidas no debe faltar.",
    japanese = "保護される種の％が欠落していてはいけません。"
  ),
  dplyr::tibble(
    id = "ui_hintboot",
    english = "Bootstrap samples must not be missing.",
    french = "Les échantillons bootstrap ne doivent pas être manquants.",
    spanish = "Las muestras bootstrap no deben faltar.",
    japanese = "ブートストラップサンプルが欠落していてはいけません。"
  ),
  dplyr::tibble(
    id = "ui_copy",
    english = "Copy code",
    french = "Copier le code",
    spanish = "Copiar código",
    japanese = "コードをコピー"
  ),
  dplyr::tibble(
    id = "ui_3conc",
    english = "Concentration",
    french = "Concentration",
    spanish = "Concentración",
    japanese = "濃度"
  ),
  dplyr::tibble(
    id = "ui_thresh_type",
    english = "Get estimate by",
    french = "Obtenir l’estimation par",
    spanish = "Obtener estimación por",
    japanese = "推定値を取得"
  ),
  dplyr::tibble(
    id = "ui_checkHc",
    english = "Plot Threshold/Concentration",
    french = "Graphique seuil/concentration",
    spanish = "Gráfico Umbral/Concentración",
    japanese = "閾値/濃度のプロット"
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
    french = "Table",
    spanish = "Tabla",
    japanese = "テーブル"
  ),
  dplyr::tibble(
    id = "ui_2rescale",
    english = "Rescale data prior to fitting",
    french = "Redimensionner les données avant l'ajustement",
    spanish = "Reescalar datos antes de ajustar",
    japanese = "適合前にデータを再スケール"
  ),
  dplyr::tibble(
    id = "ui_xmax",
    english = "X-axis maximum",
    french = "Maximum de l'axe des X",
    spanish = "Máximo del eje X",
    japanese = "X軸の最大値"
  ),
  dplyr::tibble(
    id = "ui_xmin",
    english = "X-axis minimum",
    french = "Minimum de l'axe des X",
    spanish = "Mínimo del eje X",
    japanese = "X軸の最小値"
  ),
  dplyr::tibble(
    id = "ui_xlog",
    english = "Log x-axis",
    french = "Log de l'axe X",
    spanish = "Eje X logarítmico",
    japanese = "X軸を対数"
  ),
  dplyr::tibble(
    id = "ui_sizeLabel",
    english = "Label size",
    french = "Taille de l'étiquette",
    spanish = "Tamaño de etiqueta",
    japanese = "ラベルサイズ"
  ),
  dplyr::tibble(
    id = "ui_size",
    english = "Text size",
    french = "Taille du texte",
    spanish = "Tamaño de texto",
    japanese = "テキストサイズ"
  ),
  dplyr::tibble(
    id = "ui_adjustLabel",
    english = "Shift label",
    french = "Étiquette de changement",
    spanish = "Desplazar etiqueta",
    japanese = "ラベルをシフト"
  ),
  dplyr::tibble(
    id = "ui_xbreaks",
    english = "X-axis ticks",
    french = "Repères de l'axe des X",
    spanish = "Marcas del eje X",
    japanese = "X軸の目盛り"
  ),
  dplyr::tibble(
    id = "ui_2unit",
    english = "Select units",
    french = "Sélectionner les unités",
    spanish = "Seleccionar unidades",
    japanese = "単位を選択"
  ),
  dplyr::tibble(
    id = "ui_3byconc",
    english = "by concentration",
    french = "par concentration",
    spanish = "por concentración",
    japanese = "濃度別"
  ),
  dplyr::tibble(
    id = "ui_3affecting",
    english = "affecting % species",
    french = "affectant % des espèces",
    spanish = "afectando % de especies",
    japanese = "影響を受ける種の％"
  ),
  dplyr::tibble(
    id = "ui_3protecting",
    english = "protecting % species",
    french = "protégeant % des espèces",
    spanish = "protegiendo % de especies",
    japanese = "保護される種の％"
  ),
  dplyr::tibble(
    id = "ui_1toxname",
    english = "Toxicant name (optional)",
    french = "Nom de la substance (optionnel)",
    spanish = "Nombre del tóxico (opcional)",
    japanese = "有害物質名（オプション）"
  ),
  dplyr::tibble(
    id = "ui_4toxname",
    english = "Toxicant name (optional)",
    french = "Nom de la substance (optionnel)",
    spanish = "Nombre del tóxico (opcional)",
    japanese = "有害物質名（オプション）"
  ),
  dplyr::tibble(
    id = "ui_4download",
    english = "Download Report",
    french = "Télécharger le Rapport",
    spanish = "Descargar Informe",
    japanese = "レポートをダウンロード"
  ),
  dplyr::tibble(
    id = "ui_4pdf",
    english = "PDF file",
    french = "Fichier PDF",
    spanish = "Archivo PDF",
    japanese = "PDFファイル"
  ),
  dplyr::tibble(
    id = "ui_4html",
    english = "HTML file",
    french = "Fichier HTML",
    spanish = "Archivo HTML",
    japanese = "HTMLファイル"
  ),
  dplyr::tibble(
    id = "ui_4rmd",
    english = "RMD file",
    french = "Fichier RMD",
    spanish = "Archivo RMD",
    japanese = "RMDファイル"
  ),
  dplyr::tibble(
    id = "ui_4gentitle",
    english = "Generating report ...",
    french = "Génération du rapport en cours ... ",
    spanish = "Generando informe ...",
    japanese = "レポートを生成中..."
  ),
  dplyr::tibble(
    id = "ui_4genbody",
    english = "This may take a minute, depending on the number of bootstrap samples selected.",
    french = "Cela peut prendre une minute, en fonction du nombre d'échantillons bootstrap sélectionné.",
    spanish = "Esto puede tomar un minuto, dependiendo del número de muestras bootstrap seleccionadas.",
    japanese = "選択したブートストラップサンプル数によっては、1分程度かかる場合があります。"
  ),
  dplyr::tibble(
    id = "ui_1htconc",
    english = "Conc",
    french = "Conc",
    spanish = "Conc",
    japanese = "濃度"
  ),
  dplyr::tibble(
    id = "ui_1htspp",
    english = "Species",
    french = "Espèce",
    spanish = "Especies",
    japanese = "種"
  ),
  dplyr::tibble(
    id = "ui_1htgrp",
    english = "Group",
    french = "Groupe",
    spanish = "Grupo",
    japanese = "グループ"
  ),
  dplyr::tibble(
    id = "ui_1htchm",
    english = "Chemical",
    french = "Produit Chimique",
    spanish = "Químico",
    japanese = "化学物質"
  ),
  dplyr::tibble(
    id = "ui_1htunt",
    english = "Units",
    french = "Unités",
    spanish = "Unidades",
    japanese = "単位"
  ),
  dplyr::tibble(
    id = "ui_bcanz_file",
    english = "bcanz_report.Rmd",
    french = "bcanz_report_fr.Rmd",
    spanish = "bcanz_report.Rmd",
    japanese = "bcanz_report.Rmd"
  ),
  dplyr::tibble(
    id = "ui_bcanz_filename",
    english = "bcanz_report",
    french = "rapport_bcanz",
    spanish = "informe_bcanz",
    japanese = "bcanzレポート"
  ),
  dplyr::tibble(
    id = "ui_update_fit",
    english = "Update Fit",
    french = "Mettre à jour l'ajustement",
    spanish = "Actualizar Ajuste",
    japanese = "適合を更新"
  ),
  dplyr::tibble(
    id = "ui_update_data",
    english = "Update",
    french = "Mettre à jour",
    spanish = "Actualizar",
    japanese = "更新"
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
