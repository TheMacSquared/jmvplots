#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
heatmapClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "heatmapClass",
        inherit = heatmapBase,
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

                value <- self$options$value
                if (is.null(value)) {
                    # no value variable: cells show counts per (x, y) combination
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y)
                        ) |>
                        dplyr::filter(!is.na(x) & !is.na(y)) |>
                        dplyr::mutate(x = factor(x), y = factor(y)) |>
                        dplyr::count(x, y, name = "value")
                } else {
                    # aggregate the value variable per (x, y) combination
                    aggFun <- switch(
                        self$options$aggregation,
                        sum = sum,
                        median = median,
                        mean
                    )
                    df <- self$data |>
                        dplyr::select(
                            x = !!sym(self$options$x),
                            y = !!sym(self$options$y),
                            value = !!sym(value)
                        ) |>
                        dplyr::filter(!is.na(x) & !is.na(y)) |>
                        dplyr::mutate(
                            x = factor(x),
                            y = factor(y),
                            value = jmvcore::toNumeric(value)
                        ) |>
                        dplyr::group_by(x, y) |>
                        dplyr::summarise(
                            value = aggFun(value, na.rm = TRUE),
                            .groups = "drop"
                        )
                }

                image$setState(df)
            },
            .heatmapPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                p <- ggplot(image$state, aes(x = x, y = y, fill = value)) +
                    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
                    ggplot2::scale_x_discrete(labels = jmvcore::wrapLabels) +
                    ggplot2::scale_y_discrete(labels = jmvcore::wrapLabels) +
                    ggtheme +
                    # the continuous fill scale must come AFTER ggtheme, which
                    # carries discrete colour/fill scales that would override it
                    paletteFillGradient(theme) +
                    # the gradient legend follows the user's legend options
                    formatLegend(self$options)

                if (self$options$showValues) {
                    p <- p + ggplot2::geom_text(
                        aes(label = format(
                            round(value, self$options$valueDigits),
                            trim = TRUE
                        )),
                        size = 3.2,
                        color = "grey20"
                    )
                }

                # a colourbar squeezed to the key size is unreadable, so
                # stretch it to 6x the key size along its direction while
                # keeping the key options as the thickness
                if (self$options$legenPositionType != "hide") {
                    horizontal <-
                        (self$options$legenPositionType == "outside" &&
                            self$options$legendPosition %in% c("top", "bottom")) ||
                        (self$options$legenPositionType == "inside" &&
                            self$options$legendDirection == "horizontal")
                    if (horizontal) {
                        barw <- ggplot2::unit(self$options$legendKeyWidth * 6, "cm")
                        barh <- ggplot2::unit(self$options$legendKeyHeight, "cm")
                    } else {
                        barw <- ggplot2::unit(self$options$legendKeyWidth, "cm")
                        barh <- ggplot2::unit(self$options$legendKeyHeight * 6, "cm")
                    }
                    p <- p + ggplot2::guides(
                        fill = ggplot2::guide_colorbar(
                            barwidth = barw,
                            barheight = barh
                        )
                    )
                }

                if (is.null(self$options$value)) {
                    groupLabel <- .("Count")
                } else {
                    groupLabel <- self$options$value
                }

                labelDefaults <- list(
                    xLabel = self$options$x,
                    yLabel = self$options$y,
                    groupLabel = groupLabel
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
