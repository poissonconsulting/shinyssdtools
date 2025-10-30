## Ajustement des distributions


1. Spécifier **quelle est la colonne qui contient les valeurs de concentrations**. 
L'application tente de deviner quelle est la colonne contenant les valeurs de concentrations à l'aide des noms des colonnes. 
Cela peut toutefois nécessiter une correction. 
2. **Sélectionner (ou désélectionner) les distributions à ajuster aux données.** Le graphique des distributions ajustées comprend les estimations moyennes du modèle. 
Il est à noter que s'il y a un chevauchement dans l'ajustement de deux ou plusieurs fonctions de distribution, il y a aura alors une exagération de la forme de cet ajustement dans l'inférence multimodèle. Consultez [cet article](https://bcgov.github.io/ssdtools/articles/distributions.html) pour plus d'information.  
La fonction peut prendre quelques secondes pour se mettre à jour. 
3. Mettre en forme le tracé du graphique à l'aide des entrées de la barre latérale et **télécharger le graphique et le tableau de l'évaluation de la qualité de l'ajustement des courbes de distribution** sous forme de fichiers .png et .csv respectivement. 
Sélectionner les unités pour les afficher dans le titre de l'axe des x.

Information additionnelle sur **le tableau de l'évaluation de la qualité de l'ajustement des courbes de distribution**:
Les colonnes du tableau sont la distribution (dist), la statistique d'Anderson-Darling (ad), la statistique de Kolmogorov-Smirnov (ks), la statistique de Cramer-von-Mises (cmv), le critère d'information Akaike (aic), le critère d'information Akaike corrigé pour la taille de l'échantillon (aicc), le critère d'information Bayésien (bic), la différence AICc (delta) et le coefficient AICc basé sur le poids Akaike (weight). 
L'estimation de la fonction de distribution finale  est basée sur l'inférence multimodèle (à partir de l'AICc). 
La concentration présentant un risque est la concentration estimée d'une substance affectant le pourcentage sélectionné de l'ensemble des espèces.
