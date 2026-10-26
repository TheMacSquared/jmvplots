#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
discbarClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "discbarClass",
        inherit = discbarBase,
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

                x <- jmvcore::toNumeric(self$data[[var]])
                g <- if (is.null(group)) factor("all") else factor(self$data[[group]])
                g <- rep_len(g, length(x))
                keep <- !is.na(x) & !is.na(g)
                x <- x[keep]
                g <- droplevels(g[keep])

                if (length(x) == 0) {
                    stop("Brak obserwacji dla wykresu.", call. = FALSE)
                }
                if (any(abs(x - round(x)) > 1e-8)) {
                    stop(
                        sprintf(
                            "Zmienna „%s” ma wartości niecałkowite. Ten wykres jest dla zmiennych dyskretnych (wartości całkowite); dla zmiennej ciągłej użyj histogramu.",
                            var
                        ),
                        call. = FALSE
                    )
                }
                x <- round(x)

                # every integer between the minimum and the maximum, so values
                # absent from the data get a zero-height bar
                values <- seq(min(x), max(x))
                counts <- as.data.frame(
                    table(x = factor(x, levels = values), group = g),
                    responseName = "n"
                )
                counts$x <- values[as.integer(counts$x)]

                # proportions of the whole sample, or of each group
                totals <- tapply(counts$n, counts$group, sum)
                counts$prop <- counts$n / totals[as.character(counts$group)]

                image$setState(counts)
            },
            .discbarPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state
                hasGroup <- !is.null(self$options$group)
                facet <- hasGroup && self$options$facet
                isProp <- self$options$yType == "prop"
                df$y <- if (isProp) df$prop else df$n

                if (hasGroup) {
                    # wrap long legend labels (colours still come from ggtheme)
                    wrapped <- jmvcore::wrapLabels(levels(df$group))
                    if (!anyDuplicated(wrapped)) {
                        levels(df$group) <- wrapped
                    }
                }

                # groups side by side within each value: shift every group
                # by its own offset (the same for bars and stems)
                k <- if (hasGroup && !facet) nlevels(df$group) else 1
                span <- if (k > 1) 0.8 else 0.6
                offset <- if (k > 1) (as.integer(df$group) - (k + 1) / 2) * span / k else 0
                df$xpos <- df$x + offset

                if (hasGroup) {
                    mapping <- aes(x = xpos, y = y, fill = group, color = group)
                } else {
                    mapping <- aes(x = xpos, y = y)
                }
                p <- ggplot(df, mapping)

                if (self$options$geom == "bars") {
                    fixed <- if (hasGroup) list(color = theme$color[1]) else list(fill = theme$fill[2], color = theme$color[1])
                    # zero-height bars would leave an outline on the axis
                    p <- p + do.call(ggplot2::geom_col, c(list(
                        data = df[df$n > 0, , drop = FALSE], width = span / k
                    ), fixed))
                } else {
                    fixed <- if (hasGroup) list() else list(color = theme$fill[2])
                    # no point where the value does not occur
                    df$ypoint <- ifelse(df$n > 0, df$y, NA_real_)
                    p <- p +
                        do.call(ggplot2::geom_linerange, c(list(
                            mapping = aes(ymin = 0, ymax = y), linewidth = 1
                        ), fixed)) +
                        do.call(ggplot2::geom_point, c(list(
                            data = df, mapping = aes(y = ypoint), size = 3, na.rm = TRUE
                        ), fixed))
                }

                # integer breaks only: every value when there are few of them
                values <- sort(unique(df$x))
                if (length(values) <= 15) {
                    xBreaks <- values
                } else {
                    xBreaks <- pretty(range(values), n = 8)
                    xBreaks <- xBreaks[xBreaks == round(xBreaks)]
                }
                p <- p + ggplot2::scale_x_continuous(
                    breaks = xBreaks,
                    labels = scales::label_number(big.mark = ""),
                    expand = ggplot2::expansion(add = 0.6)
                )

                heightPx <- if (self$options$flipAxes) image$width else image$height
                p <- p + ggplot2::scale_y_continuous(
                    breaks = scales::breaks_pretty(n = max(2, floor(heightPx / 80))),
                    labels = if (isProp) scales::percent else scales::label_number(big.mark = ""),
                    expand = ggplot2::expansion(mult = c(0, 0.05))
                )

                if (facet) {
                    p <- p + ggplot2::facet_wrap(ggplot2::vars(group), ncol = 1)
                }

                if (self$options$flipAxes) {
                    p <- p + ggplot2::coord_flip()
                }

                labelDefaults <- list(
                    xLabel = self$options$var,
                    yLabel = if (isProp) .("Proportion") else .("Count"),
                    groupLabel = self$options$group
                )

                p <- p +
                    ggtheme +
                    formatLegend(self$options) +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options, flipAxes = self$options$flipAxes)

                if (facet) {
                    # the panel strips already name the groups
                    p <- p + ggplot2::theme(legend.position = "none")
                }

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
