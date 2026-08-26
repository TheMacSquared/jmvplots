#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
densClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "densClass",
        inherit = densBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$var)) {
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
                        dplyr::select(y = !!sym(self$options$var)) |>
                        dplyr::mutate(y = jmvcore::toNumeric(y))
                } else {
                    df <- self$data |>
                        dplyr::select(y = !!sym(self$options$var), group = !!sym(group)) |>
                        dplyr::mutate(y = jmvcore::toNumeric(y), group = factor(group))
                }

                image$setState(df)
            },
            .densPlot = function(image, ggtheme, theme, ...) {
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
                    p <- ggplot(state, aes(x = y)) +
                        ggplot2::geom_density(
                            adjust = self$options$bwAdjust,
                            color = theme$color[1],
                            fill = theme$fill[2],
                            alpha = self$options$densityOpacity,
                            size = self$options$densityLineSize
                        ) +
                        ggtheme
                } else {
                    p <- ggplot(
                        state,
                        aes(x = y, fill = group, color = group)
                    ) +
                        ggplot2::geom_density(
                            position = "identity",
                            adjust = self$options$bwAdjust,
                            alpha = self$options$densityOpacity,
                            size = self$options$densityLineSize
                        ) +
                        ggtheme +
                        formatLegend(self$options)

                    if (self$options$densFacet) {
                        p <- p +
                            ggplot2::facet_wrap(
                                ~ group,
                                ncol = 1
                                # facet strips use the group levels wrapped above
                            )
                    }
                }

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
                    xLabel = self$options$var,
                    yLabel = .("Density"),
                    groupLabel = self$options$group
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
