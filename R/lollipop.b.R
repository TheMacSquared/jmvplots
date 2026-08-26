#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
lollipopClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "lollipopClass",
        inherit = lollipopBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$x)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                x <- self$options$x
                y <- self$options$y

                if (is.null(y)) {
                    # no value variable: count cases per level of x
                    df <- self$data |>
                        dplyr::select(x = !!sym(x)) |>
                        dplyr::filter(!is.na(x)) |>
                        dplyr::count(x, name = "value")
                } else {
                    # aggregate the value variable per level of x
                    aggFun <- switch(self$options$aggregation,
                        mean = mean,
                        sum = sum,
                        median = median
                    )

                    df <- self$data |>
                        dplyr::select(x = !!sym(x), y = !!sym(y)) |>
                        dplyr::mutate(y = jmvcore::toNumeric(y)) |>
                        dplyr::filter(!is.na(x) & !is.na(y)) |>
                        dplyr::group_by(x) |>
                        dplyr::summarise(value = aggFun(y, na.rm = TRUE), .groups = "drop")
                }

                df <- df |>
                    dplyr::mutate(x = factor(x))

                if (self$options$sortBars) {
                    # reorder levels by value, descending; with flipped axes
                    # coord_flip puts the first level at the bottom, so the
                    # order is reversed there to keep the largest value on top
                    lev <- df$x[order(-df$value)]
                    if (self$options$flipAxes)
                        lev <- rev(lev)
                    df$x <- factor(df$x, levels = lev)
                }

                image$setState(df)
            },
            .lollipopPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state

                p <- ggplot(df, aes(x = x, y = value)) +
                    ggplot2::geom_segment(
                        aes(xend = x, y = 0, yend = value),
                        color = theme$color[1],
                        linewidth = self$options$lineWidth
                    ) +
                    ggplot2::geom_point(
                        size = self$options$pointSize,
                        shape = 21,
                        color = theme$color[1],
                        fill = theme$fill[2],
                        stroke = 1
                    ) +
                    ggtheme

                if (self$options$xAxisLabelFontSizeRevLabels) {
                    p <- p + ggplot2::scale_x_discrete(limits = rev, labels = jmvcore::wrapLabels)
                } else {
                    p <- p + ggplot2::scale_x_discrete(labels = jmvcore::wrapLabels)
                }

                labelDefaults <- private$.getDefaultLabels()
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults, legend = FALSE) +
                    formatLabels(
                        options = self$options,
                        flipAxes = self$options$flipAxes,
                        legend = FALSE
                    )

                ylims <- NULL
                if (self$options$yAxisRangeType == "manual") {
                    ylims <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }

                if (self$options$flipAxes) {
                    p <- p + ggplot2::coord_flip(ylim = ylims)
                } else {
                    p <- p + ggplot2::coord_cartesian(ylim = ylims)
                }

                p <- autoscalePlotBreaks(p, image$width, image$height)
                return(p)
            },
            #### Helper functions ----
            .getDefaultLabels = function() {
                if (is.null(self$options$y)) {
                    yLabel <- .("Count")
                } else {
                    yLabel <- self$options$y
                }

                return(list(
                    xLabel = self$options$x,
                    yLabel = yLabel,
                    groupLabel = NULL
                ))
            }
        ),
        public = list(
            asSource = function() {
                return(.("Syntax mode for plots is not yet available."))
            }
        )
    )
}
