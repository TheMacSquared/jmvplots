#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
bubbleClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "bubbleClass",
        inherit = bubbleBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$x) ||
                    is.null(self$options$y) ||
                    is.null(self$options$size)) {
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
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                            size = !!sym(self$options$size)
                        ) |>
                        dplyr::mutate(
                            x = jmvcore::toNumeric(x),
                            y = jmvcore::toNumeric(y),
                            size = jmvcore::toNumeric(size)
                        )
                } else {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                            size = !!sym(self$options$size),
                            group = !!sym(group)
                        ) |>
                        dplyr::mutate(
                            x = jmvcore::toNumeric(x),
                            y = jmvcore::toNumeric(y),
                            size = jmvcore::toNumeric(size),
                            group = factor(group)
                        )
                }

                image$setState(df)
            },
            .bubblePlot = function(image, ggtheme, theme, ...) {
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
                    p <- ggplot(state, aes(x = x, y = y, size = size)) +
                        ggplot2::geom_point(
                            alpha = self$options$pointOpacity,
                            color = theme$color[1],
                            fill = theme$fill[2],
                            shape = 21
                        )
                } else {
                    p <- ggplot(state, aes(x = x, y = y, size = size, fill = group)) +
                        ggplot2::geom_point(
                            alpha = self$options$pointOpacity,
                            shape = 21,
                            color = theme$color[1]
                        )
                }

                # the size legend keeps the size variable's name as its title;
                # formatLegend applies in both branches since the size legend
                # is present even without a grouping variable
                p <- p +
                    ggplot2::scale_size_area(
                        max_size = self$options$maxSize,
                        name = if (!is.null(self$options$size)) self$options$size else NULL
                    ) +
                    ggtheme +
                    formatLegend(self$options)

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
