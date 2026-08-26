#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
stripmeanClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "stripmeanClass",
        inherit = stripmeanBase,
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
                group <- self$options$group

                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::select(y = !!sym(var)) |>
                        dplyr::mutate(x = factor(""))
                } else {
                    df <- self$data |>
                        dplyr::select(x = !!sym(group), y = !!sym(var)) |>
                        dplyr::mutate(x = factor(x))
                }

                df <- df |>
                    dplyr::mutate(y = jmvcore::toNumeric(y))

                image$setState(df)
            },
            .stripPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                data <- image$state
                if (self$options$naOmit) {
                    data <- data |>
                        dplyr::filter(!is.na(x) & !is.na(y))
                }

                p <- ggplot(data, aes(x = x, y = y)) +
                    ggplot2::geom_jitter(
                        width = self$options$jitterWidth,
                        height = 0,
                        size = self$options$pointSize,
                        alpha = self$options$pointOpacity,
                        color = theme$color[1]
                    ) +
                    ggtheme

                if (self$options$showMean) {
                    # summary statistics computed explicitly (instead of
                    # stat_summary) so the calculation is easy to inspect
                    ciWidth <- self$options$ciWidth
                    stats <- data |>
                        dplyr::filter(!is.na(y)) |>
                        dplyr::group_by(x) |>
                        dplyr::summarise(
                            n = dplyr::n(),
                            mean = mean(y),
                            sd = stats::sd(y),
                            .groups = "drop"
                        ) |>
                        dplyr::mutate(
                            se = sd / sqrt(n),
                            ci = stats::qt(1 - (1 - ciWidth / 100) / 2, n - 1) * se
                        )

                    errorBars <- self$options$errorBars
                    if (errorBars == "ci") {
                        stats$err <- stats$ci
                    } else if (errorBars == "se") {
                        stats$err <- stats$se
                    } else if (errorBars == "sd") {
                        stats$err <- stats$sd
                    } else {
                        stats$err <- NA_real_
                    }

                    if (length(theme$color) > 1) {
                        meanColor <- theme$color[2]
                    } else {
                        meanColor <- "black"
                    }

                    # mean drawn as a thick horizontal segment
                    p <- p +
                        ggplot2::geom_errorbar(
                            data = stats,
                            aes(x = x, ymin = mean, ymax = mean),
                            width = 0.4,
                            size = 1,
                            color = meanColor,
                            inherit.aes = FALSE
                        )

                    if (errorBars != "none") {
                        p <- p +
                            ggplot2::geom_errorbar(
                                data = stats,
                                aes(x = x, ymin = mean - err, ymax = mean + err),
                                width = 0.2,
                                size = 0.7,
                                color = meanColor,
                                inherit.aes = FALSE
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
                return(list(
                    xLabel = self$options$group,
                    yLabel = self$options$var,
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
