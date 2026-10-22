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
                if (self$options$mode == "wide") {
                    if (length(self$options$vars) == 0) {
                        return()
                    }
                } else if (is.null(self$options$var)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                if (self$options$mode == "wide") {
                    # several columns: each column is one category on the x axis
                    df <- wideToLong(
                        self$data,
                        unlist(self$options$vars),
                        self$options$wideGroup,
                        listwise = self$options$naOmit
                    )
                    insertWideCountsNotice(self, df)
                    image$setState(df)
                    return()
                }

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
                g <- private$.groupVars()
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

                hasGroup2 <- !is.null(g$group1) && !is.null(g$group2)

                if (is.null(g$group1) && is.null(g$group2)) {
                    p <- ggplot(data, aes(x = x, y = y)) +
                        ggplot2::geom_violin(
                            trim = self$options$trim,
                            alpha = self$options$violinOpacity,
                            color = theme$color[1],
                            fill = theme$fill[2]
                        ) +
                        ggtheme
                } else if (is.null(g$group1) || is.null(g$group2)) {
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
                                # the constant white fill drops the fill
                                # grouping, so group explicitly to dodge
                                mapping = aes(group = interaction(x, factor(group, exclude = NULL))),
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

                if (self$options$showPoints && self$options$pointLayout == "swarm") {
                    p <- p + private$.swarmLayer(p, data, hasGroup2, image, theme)
                } else if (self$options$showPoints) {
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
            # beeswarm points at numeric positions on the discrete axis; with
            # two grouping variables each subgroup swarms around its own
            # dodged position (same layout as position_dodge(0.9))
            .swarmLayer = function(p, data, hasGroup2, image, theme) {
                pointSize <- 1.5
                ylims <- NULL
                if (self$options$yAxisRangeType == "manual") {
                    ylims <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }
                unitsPerPx <- panelUnitsPerPx(p, image, self$options$flipAxes, yRange = ylims)

                swarm <- data[!is.na(data$y) & !is.na(data$x), ]
                swarm$x <- factor(swarm$x, levels = levels(factor(data$x)))
                nLevels <- nlevels(swarm$x)
                pos <- as.integer(swarm$x)
                if (self$options$xAxisLabelFontSizeRevLabels) {
                    pos <- nLevels + 1 - pos
                }
                swarm$centre <- as.numeric(pos)
                band <- 0.45

                if (hasGroup2) {
                    # missing subgroup = its own, last slot (as ggplot2 does)
                    swarm$group <- factor(swarm$group, exclude = NULL)
                    for (i in unique(swarm$centre)) {
                        atX <- swarm$centre == i
                        present <- sort(unique(as.integer(swarm$group[atX])))
                        slot <- match(as.integer(swarm$group[atX]), present)
                        n <- length(present)
                        swarm$centre[atX] <- i + (slot - (n + 1) / 2) * 0.9 / n
                        band <- min(band, 0.45 / n)
                    }
                }

                swarm$xs <- swarm$centre
                for (centre in unique(swarm$centre)) {
                    sel <- swarm$centre == centre
                    swarm$xs[sel] <- centre + swarmOffsets(
                        swarm$y[sel],
                        unitsPerPx,
                        pointSize,
                        band = band
                    )
                }

                ggplot2::geom_point(
                    data = swarm,
                    mapping = aes(x = xs, y = y),
                    inherit.aes = FALSE,
                    size = pointSize,
                    alpha = self$options$pointOpacity,
                    color = theme$color[1]
                )
            },
            .getDefaultLabels = function() {
                g <- private$.groupVars()
                if (is.null(g$group1) && !is.null(g$group2)) {
                    xLabel <- g$group2
                    groupLabel <- NULL
                } else if (!is.null(g$group1) && is.null(g$group2)) {
                    xLabel <- g$group1
                    groupLabel <- NULL
                } else {
                    xLabel <- g$group1
                    groupLabel <- g$group2
                }

                return(list(
                    xLabel = xLabel,
                    yLabel = if (self$options$mode == "wide") .("Value") else self$options$var,
                    groupLabel = groupLabel
                ))
            },
            # grouping in the plot: in the wide format the columns take the
            # place of the first grouping variable ("" = no axis title) and
            # the grouping variable the place of the second
            .groupVars = function() {
                if (self$options$mode == "wide") {
                    return(list(group1 = "", group2 = self$options$wideGroup))
                }
                list(group1 = self$options$group1, group2 = self$options$group2)
            }
        ),
        public = list(
            asSource = function() {
                return(.("Syntax mode for plots is not yet available."))
            }
        )
    )
}
