#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
dumbbellClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "dumbbellClass",
        inherit = dumbbellBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$label) || is.null(self$options$start) ||
                    is.null(self$options$end)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                df <- data.frame(
                    label = factor(self$data[[self$options$label]]),
                    start = jmvcore::toNumeric(self$data[[self$options$start]]),
                    end = jmvcore::toNumeric(self$data[[self$options$end]])
                )
                df <- df[!is.na(df$label) & !is.na(df$start) & !is.na(df$end), ]

                # one dumbbell per category: several rows of a category are
                # averaged
                if (nrow(df) > 0) {
                    df <- stats::aggregate(cbind(start, end) ~ label, data = df, FUN = mean)
                }

                image$setState(df)
            },
            .dumbbellPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state
                if (nrow(df) == 0) {
                    stop("Wykres hantlowy wymaga obserwacji z obydwoma pomiarami.", call. = FALSE)
                }

                # categories from the bottom up, so the first one is on top
                ord <- switch(self$options$order,
                    change = order(df$end - df$start),
                    start = order(df$start),
                    end = order(df$end),
                    rev(seq_len(nrow(df)))
                )
                df$label <- factor(df$label, levels = df$label[ord])
                levels(df$label) <- jmvcore::wrapLabels(levels(df$label))

                startName <- self$options$start
                endName <- self$options$end
                points <- rbind(
                    data.frame(label = df$label, value = df$start, measure = startName),
                    data.frame(label = df$label, value = df$end, measure = endName)
                )
                points$measure <- factor(points$measure, levels = unique(c(startName, endName)))

                p <- ggplot(df, aes(y = label)) +
                    ggplot2::geom_segment(
                        aes(x = start, xend = end, yend = label),
                        color = "grey60",
                        linewidth = self$options$lineWidth
                    ) +
                    ggplot2::geom_point(
                        data = points,
                        mapping = aes(x = value, y = label, color = measure),
                        size = self$options$pointSize
                    )

                labelDefaults <- list(
                    xLabel = .("Value"),
                    yLabel = "",
                    groupLabel = ""
                )

                p <- p +
                    ggtheme +
                    formatLegend(self$options) +
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
