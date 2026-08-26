#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
qqClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "qqClass",
        inherit = qqBase,
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

                    if (self$options$standardize) {
                        df <- df |>
                            dplyr::mutate(y = as.numeric(scale(y)))
                    }
                } else {
                    df <- self$data |>
                        dplyr::select(y = !!sym(self$options$var), group = !!sym(group)) |>
                        dplyr::mutate(y = jmvcore::toNumeric(y), group = factor(group))

                    if (self$options$standardize) {
                        # standardize within each group
                        df <- df |>
                            dplyr::group_by(group) |>
                            dplyr::mutate(y = as.numeric(scale(y))) |>
                            dplyr::ungroup()
                    }
                }

                image$setState(df)
            },
            .qqPlot = function(image, ggtheme, theme, ...) {
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
                    p <- ggplot(state, aes(sample = y)) +
                        ggplot2::stat_qq(
                            color = theme$color[1],
                            size = self$options$pointSize,
                            alpha = self$options$pointOpacity
                        )

                    if (self$options$qqLine) {
                        p <- p +
                            ggplot2::stat_qq_line(
                                color = theme$color[1]
                            )
                    }

                    p <- p + ggtheme
                } else {
                    p <- ggplot(
                        state,
                        aes(sample = y, color = group)
                    ) +
                        ggplot2::stat_qq(
                            size = self$options$pointSize,
                            alpha = self$options$pointOpacity
                        )

                    if (self$options$qqLine) {
                        p <- p + ggplot2::stat_qq_line()
                    }

                    p <- p + ggtheme + formatLegend(self$options)

                    if (self$options$qqFacet) {
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

                p <- p + ggplot2::coord_cartesian(ylim = ylims, xlim = xlims)

                labelDefaults <- list(
                    xLabel = .("Theoretical quantiles"),
                    yLabel = .("Sample quantiles"),
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
