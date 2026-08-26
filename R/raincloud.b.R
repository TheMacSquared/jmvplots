#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
raincloudClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "raincloudClass",
        inherit = raincloudBase,
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
            .raincloudPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                data <- image$state
                if (self$options$naOmit) {
                    data <- data |>
                        dplyr::filter(!is.na(x) & !is.na(y))
                }
                data <- data |> dplyr::filter(!is.na(y))
                if (nrow(data) == 0) {
                    return(FALSE)
                }

                levelsX <- levels(data$x)
                wrapped <- jmvcore::wrapLabels(levelsX)
                if (!anyDuplicated(wrapped)) {
                    levels(data$x) <- wrapped
                    levelsX <- wrapped
                }
                if (self$options$xAxisLabelFontSizeRevLabels) {
                    data$x <- factor(data$x, levels = rev(levelsX))
                    levelsX <- rev(levelsX)
                }

                nGroups <- length(levelsX)
                data$pos <- as.integer(data$x)
                hasGroup <- !is.null(self$options$group)

                # layout: half-violin cloud to the right of each position,
                # narrow box at the position, jittered points ("rain") left
                cloudBase <- 0.06
                cloudMax <- 0.35 * self$options$cloudScale
                pointShift <- 0.22

                # cloud polygons from kernel densities, one per group level
                cloudParts <- lapply(seq_len(nGroups), function(i) {
                    v <- data$y[data$pos == i]
                    if (length(v) < 2 || stats::sd(v) == 0)
                        return(NULL)
                    d <- stats::density(v)
                    w <- d$y / max(d$y) * cloudMax
                    # outline out along the density, then straight back down
                    poly <- data.frame(
                        xx = c(i + cloudBase + w, rep(i + cloudBase, length(w))),
                        yy = c(d$x, rev(d$x)),
                        grp = levelsX[i]
                    )
                    poly
                })
                clouds <- do.call(rbind, cloudParts)

                p <- ggplot(data)

                if (!is.null(clouds)) {
                    clouds$grp <- factor(clouds$grp, levels = levelsX)
                    if (hasGroup) {
                        p <- p +
                            ggplot2::geom_polygon(
                                data = clouds,
                                mapping = aes(x = xx, y = yy, group = grp, fill = grp),
                                color = theme$color[1],
                                linewidth = 0.4,
                                alpha = 0.8
                            ) +
                            ggplot2::guides(fill = "none")
                    } else {
                        p <- p +
                            ggplot2::geom_polygon(
                                data = clouds,
                                mapping = aes(x = xx, y = yy, group = grp),
                                color = theme$color[1],
                                fill = theme$fill[2],
                                linewidth = 0.4,
                                alpha = 0.8
                            )
                    }
                }

                if (self$options$showBox) {
                    p <- p +
                        ggplot2::geom_boxplot(
                            mapping = aes(x = pos - 0.02, y = y, group = pos),
                            width = self$options$boxWidth,
                            outlier.shape = NA,
                            fill = "white",
                            color = theme$color[1]
                        )
                }

                if (self$options$showPoints) {
                    jw <- self$options$jitterWidth
                    rain <- data
                    rain$xj <- rain$pos - pointShift +
                        stats::runif(nrow(rain), -jw, jw)
                    if (hasGroup) {
                        p <- p +
                            ggplot2::geom_point(
                                data = rain,
                                mapping = aes(x = xj, y = y, color = x),
                                size = self$options$pointSize,
                                alpha = self$options$pointOpacity
                            ) +
                            ggplot2::guides(color = "none")
                    } else {
                        p <- p +
                            ggplot2::geom_point(
                                data = rain,
                                mapping = aes(x = xj, y = y),
                                color = theme$color[1],
                                size = self$options$pointSize,
                                alpha = self$options$pointOpacity
                            )
                    }
                }

                p <- p +
                    ggplot2::scale_x_continuous(
                        breaks = seq_len(nGroups),
                        labels = levelsX,
                        limits = c(
                            1 - pointShift - 0.25,
                            nGroups + cloudBase + cloudMax + 0.1
                        )
                    ) +
                    ggtheme

                ylims <- NULL
                if (self$options$yAxisRangeType == "manual") {
                    ylims <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                }

                if (self$options$flipAxes) {
                    p <- p + ggplot2::coord_flip(ylim = ylims)
                } else {
                    p <- p + ggplot2::coord_cartesian(ylim = ylims)
                }

                labelDefaults <- list(
                    xLabel = self$options$group,
                    yLabel = self$options$var,
                    groupLabel = NULL
                )
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults, legend = FALSE) +
                    formatLabels(
                        options = self$options,
                        flipAxes = self$options$flipAxes,
                        legend = FALSE
                    )

                # no autoscalePlotBreaks here: it would overwrite the manual
                # category breaks on the continuous x scale
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
