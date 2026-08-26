#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
corrgramClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "corrgramClass",
        inherit = corrgramBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (length(self$options$vars) < 2) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                vars <- self$options$vars

                df <- self$data |>
                    dplyr::select(dplyr::all_of(vars)) |>
                    dplyr::mutate(dplyr::across(dplyr::everything(), jmvcore::toNumeric))

                corMat <- cor(
                    df,
                    use = "pairwise.complete.obs",
                    method = self$options$method
                )

                # melt the matrix to long format; as.vector() runs column-major,
                # so var1 tracks the rows and var2 tracks the columns
                n <- length(vars)
                long <- data.frame(
                    var1 = rep(vars, times = n),
                    var2 = rep(vars, each = n),
                    r = as.vector(corMat),
                    stringsAsFactors = FALSE
                )

                if (self$options$shape == "lower") {
                    # keep the lower triangle including the diagonal: cells
                    # where the y variable comes at or after the x variable
                    # in the user's selection order
                    keep <- match(long$var2, vars) >= match(long$var1, vars)
                    long <- long[keep, , drop = FALSE]
                }

                # keep the user's variable order; reverse the y levels so the
                # diagonal runs from the top-left corner
                long$var1 <- factor(long$var1, levels = vars)
                long$var2 <- factor(long$var2, levels = rev(vars))

                image$setState(long)
            },
            .corrgramPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                # diverging scale anchored at r = 0 built from the theme palette
                kols <- jmvcore::colorPalette(2, theme$palette, 'fill')

                p <- ggplot(image$state, aes(x = var1, y = var2, fill = r)) +
                    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
                    ggplot2::scale_x_discrete(labels = jmvcore::wrapLabels) +
                    ggplot2::scale_y_discrete(labels = jmvcore::wrapLabels) +
                    ggtheme +
                    # the continuous fill scale must come AFTER ggtheme, which
                    # carries discrete colour/fill scales that would override it
                    ggplot2::scale_fill_gradient2(
                        low = kols[1],
                        mid = "white",
                        high = kols[2],
                        limits = c(-1, 1)
                    ) +
                    # the gradient legend follows the user's legend options
                    formatLegend(self$options)

                if (self$options$showCoef) {
                    fmt <- paste0("%.", self$options$coefDigits, "f")
                    p <- p + ggplot2::geom_text(
                        aes(label = sprintf(fmt, r)),
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

                labelDefaults <- list(
                    xLabel = "",
                    yLabel = "",
                    groupLabel = "r"
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
