#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
circbarClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "circbarClass",
        inherit = circbarBase,
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
                y <- self$options$y

                if (is.null(y)) {
                    # no value variable: count cases per level of x
                    df <- self$data |>
                        dplyr::select(x = !!sym(x)) |>
                        dplyr::filter(!is.na(x)) |>
                        dplyr::count(x, name = "value")
                } else {
                    # aggregate the value variable per level of x
                    aggFun <- switch(self$options$aggregation,
                        mean = mean,
                        sum = sum,
                        median = median
                    )

                    df <- self$data |>
                        dplyr::select(x = !!sym(x), y = !!sym(y)) |>
                        dplyr::mutate(y = jmvcore::toNumeric(y)) |>
                        dplyr::filter(!is.na(x) & !is.na(y)) |>
                        dplyr::group_by(x) |>
                        dplyr::summarise(value = aggFun(y, na.rm = TRUE), .groups = "drop")
                }

                df <- df |>
                    dplyr::mutate(x = factor(x))

                if (self$options$sortBars) {
                    # reorder levels by value, descending
                    df$x <- factor(df$x, levels = df$x[order(-df$value)])
                }

                image$setState(df)
            },
            .circbarPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state
                maxValue <- max(df$value)

                # bars coloured per category from the plot theme palette;
                # no legend needed, categories are labelled on the axis
                p <- ggplot(df, aes(x = x, y = value, fill = x)) +
                    ggplot2::geom_col(
                        width = self$options$barWidth,
                        color = theme$color[1],
                        linewidth = 0.3
                    ) +
                    ggplot2::guides(fill = "none") +
                    ggplot2::scale_x_discrete(labels = jmvcore::wrapLabels)

                if (self$options$valueLabels) {
                    # value labels above the bar ends (added before coord_polar)
                    p <- p +
                        ggplot2::geom_text(
                            aes(label = format(round(value, 1), trim = TRUE)),
                            y = df$value + 0.06 * maxValue,
                            size = 3.2
                        )
                }

                p <- p +
                    ggplot2::coord_polar(clip = "off") +
                    ggplot2::expand_limits(y = -self$options$innerHole * maxValue) +
                    ggtheme

                labelDefaults <- list(
                    title = NULL,
                    xLabel = NULL,
                    yLabel = NULL
                )
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults, legend = FALSE) +
                    formatLabels(options = self$options, legend = FALSE) +
                    # blank the y-axis elements explicitly: formatLabels sets
                    # axis.text.y explicitly and an explicit child element wins
                    # over a blanked parent
                    ggplot2::theme(
                        axis.text.y = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank()
                    )

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
