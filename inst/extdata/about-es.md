Esta aplicación ajusta distribuciones de sensibilidad de especies a datos de concentración.
La aplicación está construida a partir del paquete R [ssdtools](https://poissonconsulting.github.io/ssdtools/) y comparte la misma funcionalidad.
Se actualizará y reimplementará después de cualquier cambio relevante en ssdtools.
Se recomienda que al informar las estimaciones de HC5 generadas usando esta aplicación, se enumeren la versión de ssdtools y el nombre de las distribuciones ajustadas al conjunto de datos.

Las columnas en la tabla de bondad de ajuste son la distribución (dist), la estadística de Anderson-Darling (ad), la estadística de Kolmogorov-Smirnov (ks), la estadística de Cramer-von Mises (cvm), el Criterio de Información de Akaike (aic), el Criterio de Información de Akaike corregido para el tamaño de la muestra (aicc), el Criterio de Información Bayesiana (bic), la diferencia de AICc (delta) y el peso de Akaike basado en AICc (weight).
La predicción es la estimación promediada del modelo (usando aicc) del ajuste.
La concentración de peligro porcentual es la concentración del químico que se predice que afectará ese porcentaje de las especies probadas.

Para informar un error, comportamiento inesperado o solicitar una función, presente un [problema de GitHub aquí](https://github.com/poissonconsulting/shinyssdtools/issues).

Para citar el paquete ssdtools en publicaciones, use:
Thorley, J., Fisher, R., Fox, D., and Schwarz, C. 2025. ssdtools v2: An R package to fit Species Sensitivity Distributions. JOSS 10(105): 7492. doi:10.21105/joss.07492.

Para citar la aplicación web, use:
Seb Dalgarno (2018) ssdtools: A shiny web app to analyse species sensitivity distributions. Prepared by Poisson Consulting for the Ministry of the Environment, British Columbia. https://bcgov-env.shinyapps.io/ssdtools/

Para obtener más información sobre el uso del promediado de modelos para generar estimaciones de HC5, consulte:
[Schwarz, C.J. and A.R. Tillmanns. 2019. Improving statistical methods to derive species sensitivity distributions. Water Science Series, WSS2019-07, Province of British Columbia, Victoria.](http://a100.gov.bc.ca/appsdata/acat/documents/r57400/2_1568399094009_8398900200.pdf)
