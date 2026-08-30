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

                plot_call_list <- private$.getPlotCallList(data, theme)

                theme_call_list_args <- list()
                if (!is.null(self$options$group1) && !is.null(self$options$group2)) {
                    theme_call_list_args <- utils::modifyList(
                        theme_call_list_args,
                        getLegendThemeCallArgs(self$options)
                    )
                }
                theme_call_list_args <- utils::modifyList(
                    theme_call_list_args,
                    getLabelsThemeCallArgs(self$options, self$options$flipAxes)
                )

                p <- createPlotFromCallStack(plot_call_list) +
                    ggtheme +
                    do.call(ggplot2::theme, theme_call_list_args)

                p <- autoscalePlotBreaks(p, image$width, image$height)
                return(p)
            },
            .getPlotCallList = function(data, theme) {
                # wrap long legend labels (colours still come from ggtheme)
                if ("group" %in% colnames(data) && is.factor(data$group)) {
                    wrapped <- jmvcore::wrapLabels(levels(data$group))
                    if (!anyDuplicated(wrapped))
                        levels(data$group) <- wrapped
                }

                noGroups <- is.null(self$options$group1) && is.null(self$options$group2)
                bothGroups <- !is.null(self$options$group1) && !is.null(self$options$group2)

                if (noGroups) {
                    mapping <- aes(x = x, y = y)
                } else if (bothGroups) {
                    mapping <- aes(x = x, y = y, fill = group)
                } else {
                    # single grouping variable: one palette colour per box,
                    # no legend (the groups are already labelled on the axis)
                    mapping <- aes(x = x, y = y, fill = x)
                }

                plot_call_list <- list(
                    "ggplot" = list(
                        fun = ggplot2::ggplot,
                        args = list(data = data, mapping = mapping)
                    )
                )

                geom_args <- list(
                    notch = self$options$notch,
                    width = self$options$boxWidth,
                    outliers = self$options$outliers,
                    color = theme$color[1]
                )
                if (noGroups) {
                    geom_args$fill <- theme$fill[2]
                } else if (bothGroups) {
                    geom_args$position <- ggplot2::position_dodge2(
                        width = self$options$boxWidth,
                        preserve = "single",
                        padding = 0.3
                    )
                }

                plot_call_list$geom_boxplot <- list(
                    fun = ggplot2::geom_boxplot,
                    args = geom_args
                )

                if (!noGroups && !bothGroups) {
                    plot_call_list$guides <- list(
                        fun = ggplot2::guides,
                        args = list(fill = "none")
                    )
                }

                if (self$options$boxMean) {
                    plot_call_list$stat_summary <- list(
                        fun = ggplot2::stat_summary,
                        args = list(
                            fun = mean,
                            geom = "point",
                            shape = 15,
                            size = 3,
                            color = theme$color[1]
                        )
                    )
                }

                if (self$options$xAxisLabelFontSizeRevLabels) {
                    plot_call_list$scale_x_discrete <- list(
                        fun = ggplot2::scale_x_discrete,
                        args = list(limits = rev, labels = jmvcore::wrapLabels)
                    )
                } else {
                    plot_call_list$scale_x_discrete <- list(
                        fun = ggplot2::scale_x_discrete,
                        args = list(labels = jmvcore::wrapLabels)
                    )
                }

                labelDefaults <- private$.getDefaultLabels()
                plot_call_list$labs <- getLabsCallList(self$options, labelDefaults)

                ylims <- NULL
                if (self$options$yAxisRangeType == "manual") {
                    ylims <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }

                if (self$options$flipAxes) {
                    plot_call_list$coord_flip <- list(
                        fun = ggplot2::coord_flip,
                        args = list(ylim = ylims)
                    )
                } else {
                    plot_call_list$coord_cartesian <- list(
                        fun = ggplot2::coord_cartesian,
                        args = list(ylim = ylims)
                    )
                }

                return(plot_call_list)
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
                if (is.null(self$options$var)) {
                    return("")
                }

                data_prep_code <- generateDataPrepCode(self$options, "box")
                call_list <- private$.getPlotCallList(
                    data = self$data,
                    theme = getSyntaxThemeColors(self$options$theme, self$options$palette)
                )
                return(finalizePlotSyntax(
                    self$options,
                    call_list,
                    data_prep_code,
                    hasLegend = !is.null(self$options$group1) && !is.null(self$options$group2),
                    flipAxes = self$options$flipAxes
                ))
            }
        )
    )
}
