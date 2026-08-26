#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
violinClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "violinClass",
        inherit = violinBase,
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

                var <- self$options$var
                group1 <- self$options$group1
                group2 <- self$options$group2

                if (is.null(group1) && is.null(group2)) {
                    df <- self$data |>
                        dplyr::select(y = !!sym(var)) |>
                        dplyr::mutate(x = "")
                } else if (is.null(group1) || is.null(group2)) {
                    group <- ifelse(is.null(group1), group2, group1)

                    df <- self$data |>
                        dplyr::select(x = !!sym(group), y = !!sym(var))
                } else {
                    df <- self$data |>
                        dplyr::select(x = !!sym(group1), y = !!sym(var), group = !!sym(group2))
                }

                df <- df |>
                    dplyr::mutate(y = jmvcore::toNumeric(y))

                image$setState(df)
            },
            .violinPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                data <- image$state
                if (self$options$naOmit) {
                    data <- data |>
                        dplyr::filter(!is.na(x) & !is.na(y))

                    if ("group" %in% colnames(data)) {
                        data <- data |> dplyr::filter(!is.na(group))
                    }
                }

                # wrap long legend labels (colours still come from ggtheme)
                if ("group" %in% colnames(data) && is.factor(data$group)) {
                    wrapped <- jmvcore::wrapLabels(levels(data$group))
                    if (!anyDuplicated(wrapped))
                        levels(data$group) <- wrapped
                }

                hasGroup2 <- !is.null(self$options$group1) && !is.null(self$options$group2)

                if (is.null(self$options$group1) && is.null(self$options$group2)) {
                    p <- ggplot(data, aes(x = x, y = y)) +
                        ggplot2::geom_violin(
                            trim = self$options$trim,
                            alpha = self$options$violinOpacity,
                            color = theme$color[1],
                            fill = theme$fill[2]
                        ) +
                        ggtheme
                } else if (is.null(self$options$group1) || is.null(self$options$group2)) {
                    # single grouping variable: one palette colour per violin,
                    # no legend (the groups are already labelled on the axis)
                    p <- ggplot(data, aes(x = x, y = y, fill = x)) +
                        ggplot2::geom_violin(
                            trim = self$options$trim,
                            alpha = self$options$violinOpacity,
                            color = theme$color[1]
                        ) +
                        ggtheme +
                        ggplot2::guides(fill = "none")
                } else {
                    p <- ggplot(data, aes(x = x, y = y, fill = group)) +
                        ggplot2::geom_violin(
                            trim = self$options$trim,
                            alpha = self$options$violinOpacity,
                            color = theme$color[1],
                            position = ggplot2::position_dodge(0.9)
                        ) +
                        ggtheme +
                        formatLegend(self$options)
                }

                if (self$options$innerBox) {
                    if (hasGroup2) {
                        p <- p +
                            ggplot2::geom_boxplot(
                                width = self$options$innerBoxWidth,
                                alpha = 1,
                                outlier.shape = NA,
                                position = ggplot2::position_dodge(0.9),
                                fill = "white",
                                color = theme$color[1]
                            )
                    } else {
                        p <- p +
                            ggplot2::geom_boxplot(
                                width = self$options$innerBoxWidth,
                                alpha = 1,
                                outlier.shape = NA,
                                fill = "white",
                                color = theme$color[1]
                            )
                    }
                }

                if (self$options$showPoints) {
                    if (hasGroup2) {
                        p <- p +
                            ggplot2::geom_point(
                                position = ggplot2::position_jitterdodge(
                                    jitter.width = self$options$pointJitter,
                                    dodge.width = 0.9
                                ),
                                alpha = self$options$pointOpacity,
                                color = theme$color[1]
                            )
                    } else {
                        p <- p +
                            ggplot2::geom_jitter(
                                width = self$options$pointJitter,
                                alpha = self$options$pointOpacity,
                                color = theme$color[1]
                            )
                    }
                }

                if (self$options$xAxisLabelFontSizeRevLabels) {
                    p <- p + ggplot2::scale_x_discrete(limits = rev, labels = jmvcore::wrapLabels)
                } else {
                    p <- p + ggplot2::scale_x_discrete(labels = jmvcore::wrapLabels)
                }

                labelDefaults <- private$.getDefaultLabels()
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

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
                if (is.null(self$options$group1) && !is.null(self$options$group2)) {
                    xLabel <- self$options$group2
                    groupLabel <- NULL
                } else if (!is.null(self$options$group1) && is.null(self$options$group2)) {
                    xLabel <- self$options$group1
                    groupLabel <- NULL
                } else {
                    xLabel <- self$options$group1
                    groupLabel <- self$options$group2
                }

                return(list(
                    xLabel = xLabel,
                    yLabel = self$options$var,
                    groupLabel = groupLabel
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
