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
                if (is.null(self$options$x)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                x <- self$options$x
                # without a segments variable the X categories are the segments:
                # stacked = one bar split by X, otherwise one bar per X category
                segments <- self$options$segments
                if (is.null(segments)) {
                    segments <- x
                }
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

                if (is.null(self$options$segments) && self$options$barType == "percent") {
                    df$x <- factor("")
                }

                image$setState(df)
            },
            .stackbarPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                data <- image$state
                if (nrow(data) == 0) {
                    stop("Brak obserwacji dla wykresu słupkowego.", call. = FALSE)
                }

                # wrap long legend labels (colours still come from ggtheme)
                wrapped <- jmvcore::wrapLabels(levels(data$segments))
                if (!anyDuplicated(wrapped)) {
                    levels(data$segments) <- wrapped
                }

                noSegments <- is.null(self$options$segments)
                barType <- self$options$barType
                isStacked <- barType == "percent"
                isPercent <- barType != "count"
                outsideLabels <- isStacked &&
                    self$options$percentLabels &&
                    self$options$percentLabelsPosition == "outside"

                if (isStacked) {
                    p <- ggplot(data, aes(x = x, fill = segments, weight = w)) +
                        ggplot2::geom_bar(
                            position = "fill",
                            width = self$options$barWidth,
                            color = theme$color[1]
                        )
                } else {
                    # side by side: counts, or shares of the segments within
                    # each x category (every x sums to 100%); empty
                    # combinations are kept as zero-height bars; without
                    # segments: one bar per x, shares of the total
                    if (noSegments) {
                        barData <- as.data.frame(
                            stats::xtabs(w ~ x, data = data),
                            responseName = "n"
                        )
                        barData$segments <- barData$x
                        barData$prop <- if (sum(barData$n) > 0) barData$n / sum(barData$n) else 0
                    } else {
                        barData <- as.data.frame(
                            stats::xtabs(w ~ x + segments, data = data),
                            responseName = "n"
                        )
                        barData <- barData |>
                            dplyr::group_by(x) |>
                            dplyr::mutate(prop = if (sum(n) > 0) n / sum(n) else 0) |>
                            dplyr::ungroup()
                    }
                    barData$y <- if (isPercent) barData$prop else barData$n
                    dodge <- ggplot2::position_dodge(width = self$options$barWidth)

                    p <- ggplot(barData, aes(x = x, y = y, fill = segments)) +
                        ggplot2::geom_col(
                            position = dodge,
                            width = self$options$barWidth * 0.9,
                            color = theme$color[1]
                        )

                    if (isPercent && self$options$percentLabels) {
                        p <- p + ggplot2::geom_text(
                            mapping = aes(label = scales::percent(prop, accuracy = 1)),
                            position = dodge,
                            vjust = if (self$options$flipAxes) 0.5 else -0.4,
                            hjust = if (self$options$flipAxes) -0.15 else 0.5
                        )
                    }
                }

                if (outsideLabels && !self$options$flipAxes) {
                    # extra room on the right so outside labels are not clipped
                    p <- p + ggplot2::scale_x_discrete(
                        labels = jmvcore::wrapLabels,
                        expand = ggplot2::expansion(add = c(0.6, 1.0))
                    )
                } else {
                    p <- p + ggplot2::scale_x_discrete(labels = jmvcore::wrapLabels)
                }

                if (isStacked && self$options$percentLabels) {
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
                if (noSegments && !isStacked) {
                    # colours repeat the x axis, the legend would be redundant
                    p <- p + ggplot2::theme(legend.position = "none")
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

                if (isPercent) {
                    yLabelDefault <- .("Proportion")
                } else {
                    yLabelDefault <- .("Count")
                }

                labelDefaults <- list(
                    xLabel = if (noSegments && isStacked) "" else self$options$x,
                    yLabel = yLabelDefault,
                    groupLabel = if (noSegments) self$options$x else self$options$segments
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
