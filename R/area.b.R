#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
areaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "areaClass",
        inherit = areaBase,
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

                group <- self$options$group

                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::select(x = !!sym(self$options$x), y = !!sym(self$options$y)) |>
                        dplyr::mutate(x = jmvcore::toNumeric(x), y = jmvcore::toNumeric(y)) |>
                        dplyr::arrange(x)
                } else {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                            group = !!sym(group)
                        ) |>
                        dplyr::mutate(
                            x = jmvcore::toNumeric(x),
                            y = jmvcore::toNumeric(y),
                            group = factor(group)
                        ) |>
                        dplyr::arrange(x)
                }

                image$setState(df)
            },
            .areaPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                group <- self$options$group

                state <- image$state
                if (!is.null(group) && is.factor(state$group)) {
                    # wrap long legend labels (colours still come from ggtheme)
                    wrapped <- jmvcore::wrapLabels(levels(state$group))
                    if (!anyDuplicated(wrapped))
                        levels(state$group) <- wrapped
                }

                if (is.null(group)) {
                    p <- ggplot(state, aes(x = x, y = y)) +
                        ggplot2::geom_area(
                            color = theme$color[1],
                            fill = theme$fill[2],
                            alpha = self$options$areaOpacity,
                            size = self$options$lineSize
                        ) +
                        ggtheme
                } else {
                    position <- if (self$options$stacked) "stack" else "identity"

                    p <- ggplot(state, aes(x = x, y = y, fill = group, color = group)) +
                        ggplot2::geom_area(
                            position = position,
                            alpha = self$options$areaOpacity,
                            size = self$options$lineSize
                        ) +
                        ggtheme +
                        formatLegend(self$options)
                }

                ylims <- NULL
                if (self$options$yAxisRangeType == "manual") {
                    ylims <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }

                xlims <- NULL
                if (self$options$xAxisRangeType == "manual") {
                    xlims <- c(self$options$xAxisRangeMin, self$options$xAxisRangeMax)
                }

                p <- p + ggplot2::coord_cartesian(ylim = ylims, xlim = xlims)

                labelDefaults <- list(
                    xLabel = self$options$x,
                    yLabel = self$options$y,
                    groupLabel = self$options$group
                )
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options)

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
