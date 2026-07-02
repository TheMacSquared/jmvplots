#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
hexbinClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "hexbinClass",
        inherit = hexbinBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$x) || is.null(self$options$y)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                df <- self$data |>
                    dplyr::select(x = !!sym(self$options$x), y = !!sym(self$options$y)) |>
                    dplyr::mutate(x = jmvcore::toNumeric(x), y = jmvcore::toNumeric(y))

                image$setState(df)
            },
            .hexbinPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                p <- ggplot(image$state, aes(x = x, y = y)) +
                    ggplot2::geom_hex(bins = self$options$bins) +
                    ggtheme +
                    ggplot2::scale_fill_viridis_c() +
                    formatLegend(self$options)

                ylims <- NULL
                if (self$options$yAxisRangeType == "manual") {
                    ylims <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }

                xlims <- NULL
                if (self$options$xAxisRangeType == "manual") {
                    xlims <- c(self$options$xAxisRangeMin, self$options$xAxisRangeMax)
                }

                if (self$options$flipAxes) {
                    p <- p + ggplot2::coord_flip(ylim = ylims, xlim = xlims)
                } else {
                    p <- p + ggplot2::coord_cartesian(ylim = ylims, xlim = xlims)
                }

                labelDefaults <- list(
                    xLabel = self$options$x,
                    yLabel = self$options$y,
                    groupLabel = .("Count")
                )
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

                p <- autoscalePlotBreaks(p, image$width, image$height)

                return(p)
            }
        ),
        public = list(
            asSource = function() {
                return(.("Syntax mode for plots is not yet available."))
            }
        )
    )
}
