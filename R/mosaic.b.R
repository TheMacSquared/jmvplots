#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
mosaicClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "mosaicClass",
        inherit = mosaicBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$x) || is.null(self$options$y)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                x <- self$options$x
                y <- self$options$y
                counts <- self$options$counts

                if (is.null(counts)) {
                    df <- self$data |>
                        dplyr::select(x = !!sym(x), y = !!sym(y)) |>
                        dplyr::mutate(w = 1)
                } else {
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(x),
                            y = !!sym(y),
                            w = !!sym(counts)
                        ) |>
                        dplyr::mutate(w = jmvcore::toNumeric(w)) |>
                        dplyr::filter(!is.na(w), w > 0)
                }

                df <- df |>
                    dplyr::filter(!is.na(x), !is.na(y)) |>
                    dplyr::mutate(x = factor(x), y = factor(y)) |>
                    dplyr::group_by(x, y) |>
                    dplyr::summarize(n = sum(w), .groups = "drop")

                image$setState(df)
            },
            .mosaicPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state
                total <- sum(df$n)
                if (nrow(df) == 0 || total <= 0) {
                    return(FALSE)
                }

                # wrap long legend labels (colours still come from ggtheme)
                wrapped <- jmvcore::wrapLabels(levels(df$y))
                if (!anyDuplicated(wrapped)) {
                    levels(df$y) <- wrapped
                }

                xLevels <- levels(df$x)
                yLevels <- levels(df$y)

                # column widths proportional to the marginal totals of x;
                # within each column the tiles split by P(y | x), stacked
                # with the first y level at the top (stacked-bar convention)
                margx <- vapply(
                    xLevels,
                    function(l) sum(df$n[df$x == l]),
                    numeric(1)
                )
                keep <- margx > 0
                xLevels <- xLevels[keep]
                margx <- margx[keep]
                colW <- margx / sum(margx)
                colRight <- cumsum(colW)
                colLeft <- colRight - colW

                gap <- 0.004
                tiles <- do.call(rbind, lapply(seq_along(xLevels), function(i) {
                    sub <- df[df$x == xLevels[i] & df$n > 0, ]
                    sub <- sub[order(as.integer(sub$y)), ]
                    prop <- sub$n / sum(sub$n)
                    ymax <- 1 - c(0, cumsum(prop))[seq_along(prop)]
                    data.frame(
                        xmin = colLeft[i] + gap,
                        xmax = colRight[i] - gap,
                        ymin = ymax - prop + gap,
                        ymax = ymax - gap,
                        y = sub$y,
                        prop = prop
                    )
                }))

                p <- ggplot(
                    tiles,
                    aes(
                        xmin = xmin,
                        xmax = xmax,
                        ymin = ymin,
                        ymax = ymax,
                        fill = y
                    )
                ) +
                    ggplot2::geom_rect(color = "white", linewidth = 0.4) +
                    ggplot2::scale_x_continuous(
                        breaks = (colLeft + colRight) / 2,
                        labels = jmvcore::wrapLabels(xLevels),
                        expand = ggplot2::expansion(mult = 0.01)
                    ) +
                    ggplot2::scale_y_continuous(
                        labels = scales::percent,
                        expand = ggplot2::expansion(mult = 0.01)
                    ) +
                    ggtheme +
                    formatLegend(self$options)

                if (self$options$percentLabels) {
                    # conditional percentages P(y | x); tiles too small to
                    # hold a label are skipped
                    labelData <- tiles[
                        (tiles$ymax - tiles$ymin) >= 0.04 &
                            (tiles$xmax - tiles$xmin) >= 0.06,
                    ]
                    if (nrow(labelData) > 0) {
                        p <- p +
                            ggplot2::geom_text(
                                data = labelData,
                                mapping = aes(
                                    x = (xmin + xmax) / 2,
                                    y = (ymin + ymax) / 2,
                                    label = scales::percent(prop, accuracy = 1)
                                ),
                                inherit.aes = FALSE,
                                size = 3.2,
                                color = "grey20"
                            )
                    }
                }

                labelDefaults <- list(
                    xLabel = self$options$x,
                    yLabel = .("Proportion"),
                    groupLabel = self$options$y
                )
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options)

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
