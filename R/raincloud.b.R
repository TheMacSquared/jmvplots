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
                    df <- wideToLong(self$data, unlist(self$options$vars))
                    insertWideCountsNotice(self, df)
                    image$setState(df)
                    return()
                }

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
                    stop("Brak obserwacji bez braków danych dla wykresu deszczowego.", call. = FALSE)
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

                leftExtent <- pointShift + 0.25
                if (self$options$showPoints) {
                    rain <- data
                    if (self$options$pointLayout == "stack") {
                        # dots stacked away from the cloud: bins of equal
                        # width shared by all groups, each dot drawn at its
                        # bin centre, so stack lengths are comparable
                        yRange <- range(rain$y)
                        if (yRange[2] > yRange[1]) {
                            binWidth <- (yRange[2] - yRange[1]) / 30
                            bin <- pmin(floor((rain$y - yRange[1]) / binWidth), 29)
                            rain$y <- yRange[1] + (bin + 0.5) * binWidth
                        } else {
                            bin <- rep(0, nrow(rain))
                        }
                        # 0-based rank of each dot within its group and bin
                        stackIndex <- stats::ave(
                            seq_len(nrow(rain)),
                            rain$pos,
                            bin,
                            FUN = seq_along
                        ) - 1
                        # stacks start left of the box and may use the band
                        # up to the neighbouring group's cloud
                        stackBase <- 0.12
                        if (self$options$showBox) {
                            stackBase <- max(stackBase, self$options$boxWidth / 2 + 0.06)
                        }
                        stackBand <- max(0.15, 0.95 - stackBase - cloudBase - cloudMax)
                        # step of about one dot diameter (points are sized
                        # in mm, the axis in group units), shrunk when the
                        # longest stack would not fit in the band
                        axisPx <- 0.8 * if (self$options$flipAxes) image$height else image$width
                        dotPx <- self$options$pointSize / 25.4 * 72
                        step <- dotPx / (axisPx / (nGroups + 0.5))
                        step <- min(step, stackBand / max(stackIndex, 1))
                        rain$xj <- rain$pos - stackBase - stackIndex * step
                        leftExtent <- max(leftExtent, stackBase + stackBand + 0.05)
                    } else if (self$options$pointLayout == "swarm") {
                        # one-sided swarm hanging from the same baseline as
                        # the stacks
                        swarmBase <- 0.12
                        if (self$options$showBox) {
                            swarmBase <- max(swarmBase, self$options$boxWidth / 2 + 0.06)
                        }
                        swarmBand <- max(0.15, 0.95 - swarmBase - cloudBase - cloudMax)
                        leftExtent <- max(leftExtent, swarmBase + swarmBand + 0.05)
                        unitsPerPx <- panelUnitsPerPx(
                            p,
                            image,
                            self$options$flipAxes,
                            xRange = c(1 - leftExtent, nGroups + cloudBase + cloudMax + 0.1),
                            yRange = if (self$options$yAxisRangeType == "manual") {
                                c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
                            }
                        )
                        rain$xj <- rain$pos - swarmBase
                        for (i in seq_len(nGroups)) {
                            sel <- rain$pos == i
                            rain$xj[sel] <- rain$xj[sel] + swarmOffsets(
                                rain$y[sel],
                                unitsPerPx,
                                self$options$pointSize,
                                side = "down",
                                band = swarmBand
                            )
                        }
                    } else {
                        jw <- self$options$jitterWidth
                        rain$xj <- rain$pos - pointShift +
                            stats::runif(nrow(rain), -jw, jw)
                    }
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
                            1 - leftExtent,
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

                wide <- self$options$mode == "wide"
                labelDefaults <- list(
                    xLabel = if (wide) "" else self$options$group,
                    yLabel = if (wide) .("Value") else self$options$var,
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
