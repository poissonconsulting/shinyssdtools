## Ajustar distribuciones


1. Especifique **qué columna contiene valores de concentración**. La aplicación intenta adivinar qué columna contiene valores de concentración basándose en los nombres de las columnas de datos. Esto puede necesitar ser corregido.
2. **Seleccione o deseleccione distribuciones para ajustar los datos**.
El gráfico de distribuciones ajustadas incluye las estimaciones promedio del modelo.
Tenga en cuenta que si dos o más modelos tienen ajustes superpuestos, el soporte para esta forma de modelo estará sobreinflado en los parámetros promediados del modelo.
Consulte el artículo [aquí](https://bcgov.github.io/ssdtools/articles/distributions.html) para obtener más información.
Las salidas pueden tardar un momento en actualizarse.
3. Formatee el gráfico usando las entradas en la barra lateral y **descargue el gráfico y la tabla de bondad de ajuste** como archivos png y csv, respectivamente.
Seleccione unidades para mostrarlas en el título del eje x.

Información adicional sobre la **tabla de bondad de ajuste**:
Las columnas en la tabla de bondad de ajuste son la distribución (dist), la estadística de Anderson-Darling (ad), la estadística de Kolmogorov-Smirnov (ks), la estadística de Cramer-von Mises (cvm), el Criterio de Información de Akaike (aic), el Criterio de Información de Akaike corregido para el tamaño de la muestra (aicc), el Criterio de Información Bayesiana (bic), la diferencia de AICc (delta) y el peso de Akaike basado en AICc (weight).
La predicción es la estimación promediada del modelo (usando aicc) del ajuste.
La concentración de peligro porcentual es la concentración del químico que se predice que afectará ese porcentaje de las especies probadas.
