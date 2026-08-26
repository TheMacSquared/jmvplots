#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
ridgeClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "ridgeClass",
        inherit = ridgeBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$var) || is.null(self$options$group)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                df <- self$data |>
                    dplyr::select(x = !!sym(self$options$var), y = !!sym(self$options$group)) |>
                    dplyr::mutate(x = jmvcore::toNumeric(x), y = factor(y))

                image$setState(df)
            },
            .ridgePlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                data <- image$state
                if (self$options$naOmit) {
                    data <- data |>
                        dplyr::filter(!is.na(x) & !is.na(y))
                }

                # Reverse the factor levels so the first category sits at the top
                data$y <- factor(data$y, levels = rev(levels(data$y)))

                scale <- self$options$scale

                if (self$options$gradient) {
                    p <- ggplot(data, aes(x = x, y = y, fill = ggplot2::after_stat(x))) +
                        ggridges::geom_density_ridges_gradient(
                            scale = scale,
                            color = theme$color[1]
                        ) +
                        ggtheme +
                        paletteFillGradient(theme) +
                        # the gradient legend follows the user's legend options
                        formatLegend(self$options)

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
                } else {
                    p <- ggplot(data, aes(x = x, y = y)) +
                        ggridges::geom_density_ridges(
                            scale = scale,
                            color = theme$color[1],
                            fill = theme$fill[2]
                        ) +
                        ggtheme
                }

                p <- p + ggplot2::scale_y_discrete(labels = jmvcore::wrapLabels)

                labelDefaults <- list(
                    xLabel = self$options$var,
                    yLabel = self$options$group,
                    # the gradient legend maps the x values, so it defaults
                    # to the variable's name
                    groupLabel = self$options$var
                )
                p <- p +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options)

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
