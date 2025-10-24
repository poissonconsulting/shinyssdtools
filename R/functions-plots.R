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

plot_distributions <- function(
  x,
  ylab,
  xlab,
  text_size,
  big.mark,
  decimal.mark,
  title = NULL
) {
  gp <- ssdtools::ssd_plot_cdf(
    x,
    ylab = ylab,
    xlab = xlab,
    delta = Inf,
    average = NA,
    theme_classic = TRUE,
    text_size = text_size,
    big.mark = big.mark,
    decimal.mark = decimal.mark
  )

  if (!is.null(title) && title != "") {
    gp <- gp + ggplot2::ggtitle(title)
  }

  gp
}

gp_xbreaks <- function(gp) {
  breaks <- ggplot2::ggplot_build(gp)$layout$panel_params[[1]]$x$breaks
  sort(signif(as.numeric(stats::na.omit(breaks)), 3))
}

plot_predictions <- function(
  x,
  pred,
  conc,
  label,
  colour,
  shape,
  percent,
  label_adjust,
  xaxis,
  yaxis,
  title,
  xmin,
  xmax,
  palette,
  legend_colour,
  legend_shape,
  xbreaks = NULL,
  trans,
  text_size,
  label_size,
  conc_value,
  big.mark,
  decimal.mark
) {
  proportion <- percent / 100
  if (!length(proportion)) {
    proportion <- NULL
  }

  xlimits <- c(xmin, xmax)
  if (is.na(xmin) & is.na(xmax)) {
    xlimits <- NULL
  }

  gp <- ssdtools::ssd_plot(
    x,
    pred = pred,
    left = conc,
    label = label,
    shape = shape,
    color = colour,
    label_size = label_size,
    xlab = xaxis,
    ylab = yaxis,
    ci = FALSE,
    hc = proportion,
    shift_x = as.numeric(label_adjust),
    big.mark = big.mark,
    trans = trans,
    xlimits = xlimits,
    xbreaks = xbreaks,
    text_size = text_size,
    theme_classic = TRUE,
    decimal.mark = decimal.mark
  ) +
    ggplot2::scale_color_brewer(palette = palette, name = legend_colour) +
    ggplot2::scale_shape(name = legend_shape) +
    ggplot2::ggtitle(title)

  gp
}
