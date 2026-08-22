#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
jmvboxClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "jmvboxClass",
        inherit = jmvboxBase,
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
                        dplyr::mutate(x = "1")
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
            .boxPlot = function(image, ggtheme, theme, ...) {
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

                if (is.null(self$options$group1) && is.null(self$options$group2)) {
                    p <- ggplot(data, aes(x = x, y = y)) +
                        ggplot2::geom_boxplot(
                            notch = self$options$notch,
                            width = self$options$boxWidth,
                            outliers = self$options$outliers,
                            color = theme$color[1],
                            fill = theme$fill[2]
                        ) +
                        ggtheme
                } else if (is.null(self$options$group1) || is.null(self$options$group2)) {
                    # single grouping variable: one palette colour per box,
                    # no legend (the groups are already labelled on the axis)
                    p <- ggplot(data, aes(x = x, y = y, fill = x)) +
                        ggplot2::geom_boxplot(
                            notch = self$options$notch,
                            width = self$options$boxWidth,
                            outliers = self$options$outliers,
                            color = theme$color[1]
                        ) +
                        ggtheme +
                        ggplot2::guides(fill = "none")
                } else {
                    p <- ggplot(data, aes(x = x, y = y, fill = group)) +
                        ggplot2::geom_boxplot(
                            notch = self$options$notch,
                            width = self$options$boxWidth,
                            outliers = self$options$outliers,
                            color = theme$color[1],
                            position = ggplot2::position_dodge2(
                                width = self$options$boxWidth,
                                preserve = "single",
                                padding = 0.3
                            )
                        ) +
                        ggtheme +
                        formatLegend(self$options)
                }

                if (self$options$boxMean) {
                    p <- p +
                        ggplot2::stat_summary(
                            fun = mean,
                            geom = "point",
                            shape = 15,
                            size = 3,
                            color = theme$color[1]
                        )
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
