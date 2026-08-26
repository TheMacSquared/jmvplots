#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
stackbarClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "stackbarClass",
        inherit = stackbarBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$x) || is.null(self$options$segments)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                x <- self$options$x
                segments <- self$options$segments
                counts <- self$options$counts

                if (is.null(counts)) {
                    df <- self$data |>
                        dplyr::select(x = !!sym(x), segments = !!sym(segments)) |>
                        dplyr::mutate(w = 1)
                } else {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(x),
                            segments = !!sym(segments),
                            w = !!sym(counts)
                        ) |>
                        dplyr::mutate(w = jmvcore::toNumeric(w)) |>
                        dplyr::filter(!is.na(w))
                }

                # remove rows with missing category values
                df <- df |>
                    dplyr::filter(!is.na(x), !is.na(segments)) |>
                    dplyr::mutate(x = factor(x), segments = factor(segments))

                image$setState(df)
            },
            .stackbarPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                data <- image$state
                if (nrow(data) == 0) {
                    return(FALSE)
                }

                # wrap long legend labels (colours still come from ggtheme)
                wrapped <- jmvcore::wrapLabels(levels(data$segments))
                if (!anyDuplicated(wrapped)) {
                    levels(data$segments) <- wrapped
                }

                isPercent <- self$options$barType == "percent"
                position <- if (isPercent) "fill" else "stack"
                outsideLabels <- isPercent &&
                    self$options$percentLabels &&
                    self$options$percentLabelsPosition == "outside"

                p <- ggplot(data, aes(x = x, fill = segments, weight = w)) +
                    ggplot2::geom_bar(
                        position = position,
                        width = self$options$barWidth,
                        color = theme$color[1]
                    )

                if (outsideLabels && !self$options$flipAxes) {
                    # extra room on the right so outside labels are not clipped
                    p <- p + ggplot2::scale_x_discrete(
                        labels = jmvcore::wrapLabels,
                        expand = ggplot2::expansion(add = c(0.6, 1.0))
                    )
                } else {
                    p <- p + ggplot2::scale_x_discrete(labels = jmvcore::wrapLabels)
                }

                if (isPercent && self$options$percentLabels) {
                    # per-bar proportions (weights taken into account)
                    labelData <- data |>
                        dplyr::group_by(x, segments) |>
                        dplyr::summarize(n = sum(w), .groups = "drop_last") |>
                        dplyr::mutate(prop = n / sum(n)) |>
                        dplyr::ungroup()

                    if (self$options$percentLabelsPosition == "inside") {
                        p <- p +
                            ggplot2::geom_text(
                                mapping = aes(
                                    x = x,
                                    y = prop,
                                    group = segments,
                                    label = scales::percent(prop, accuracy = 1)
                                ),
                                data = labelData,
                                position = ggplot2::position_fill(vjust = 0.5),
                                inherit.aes = FALSE
                            )
                    } else {
                        # outside: labels beside the bar, at each segment's
                        # vertical midpoint; ggplot2 stacks the first factor
                        # level at the top, so midpoints accumulate from the top
                        labelData <- labelData |>
                            dplyr::arrange(x, segments) |>
                            dplyr::group_by(x) |>
                            dplyr::mutate(ypos = 1 - (cumsum(prop) - prop / 2)) |>
                            dplyr::ungroup() |>
                            dplyr::mutate(
                                # sit right at the bar's edge so the label
                                # clearly belongs to its own bar
                                xpos = as.integer(x) + self$options$barWidth / 2 + 0.02
                            )

                        p <- p +
                            ggplot2::geom_text(
                                mapping = aes(
                                    x = xpos,
                                    y = ypos,
                                    label = scales::percent(prop, accuracy = 1)
                                ),
                                data = labelData,
                                inherit.aes = FALSE,
                                hjust = 0
                            )
                    }
                }

                if (isPercent) {
                    p <- p + ggplot2::scale_y_continuous(labels = scales::percent)
                }

                p <- p + ggtheme + formatLegend(self$options)

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

                if (isPercent) {
                    yLabelDefault <- .("Proportion")
                } else {
                    yLabelDefault <- .("Count")
                }

                labelDefaults <- list(
                    xLabel = self$options$x,
                    yLabel = yLabelDefault,
                    groupLabel = self$options$segments
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
