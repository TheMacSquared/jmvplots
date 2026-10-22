#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
slopeClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "slopeClass",
        inherit = slopeBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$start) || is.null(self$options$end)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                df <- data.frame(
                    start = jmvcore::toNumeric(self$data[[self$options$start]]),
                    end = jmvcore::toNumeric(self$data[[self$options$end]])
                )
                if (is.null(self$options$label)) {
                    # one line per row (e.g. one student before and after)
                    df$label <- factor(seq_len(nrow(df)))
                    df <- df[!is.na(df$start) & !is.na(df$end), ]
                } else {
                    # one line per category: several rows are averaged
                    df$label <- factor(self$data[[self$options$label]])
                    df <- df[!is.na(df$label) & !is.na(df$start) & !is.na(df$end), ]
                    if (nrow(df) > 0) {
                        df <- stats::aggregate(cbind(start, end) ~ label, data = df, FUN = mean)
                    }
                }

                image$setState(df)
            },
            .slopePlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state
                if (nrow(df) == 0) {
                    stop("Wykres nachylenia wymaga obserwacji z obydwoma pomiarami.", call. = FALSE)
                }

                change <- c(.("Increase"), .("Decrease"), .("No change"))
                df$change <- factor(
                    ifelse(df$end > df$start, change[1], ifelse(df$end < df$start, change[2], change[3])),
                    levels = change
                )

                startName <- self$options$start
                endName <- self$options$end
                long <- rbind(
                    data.frame(label = df$label, change = df$change, x = 1, value = df$start),
                    data.frame(label = df$label, change = df$change, x = 2, value = df$end)
                )

                showLabels <- !is.null(self$options$label) && self$options$showLabels

                p <- ggplot(long, aes(x = x, y = value, group = label, color = change)) +
                    ggplot2::geom_line(
                        alpha = self$options$lineOpacity,
                        linewidth = self$options$lineWidth
                    ) +
                    ggplot2::geom_point(
                        alpha = self$options$lineOpacity,
                        size = 1.5 + self$options$lineWidth
                    ) +
                    ggplot2::scale_x_continuous(
                        breaks = c(1, 2),
                        labels = jmvcore::wrapLabels(c(startName, endName)),
                        expand = ggplot2::expansion(add = c(0.25, if (showLabels) 0.8 else 0.25))
                    )

                if (showLabels) {
                    labels <- long[long$x == 2, ]
                    labels$y <- private$.spreadLabels(labels$value, diff(range(long$value)) * 0.075)
                    p <- p + ggplot2::geom_text(
                        data = labels,
                        mapping = aes(y = y, label = label),
                        x = 2.06,
                        hjust = 0,
                        show.legend = FALSE
                    )
                }

                labelDefaults <- list(
                    xLabel = "",
                    yLabel = .("Value"),
                    groupLabel = ""
                )

                p <- p +
                    ggtheme +
                    formatLegend(self$options) +
                    setLabels(options = self$options, defaults = labelDefaults) +
                    formatLabels(options = self$options)

                return(p)
            },
            # Vertical positions for labels at y, at least `gap` apart: pushed
            # up in order, then shifted back so they stay centred on the points
            .spreadLabels = function(y, gap) {
                if (length(y) < 2 || !is.finite(gap) || gap <= 0) {
                    return(y)
                }
                ord <- order(y)
                pos <- y[ord]
                for (i in seq_along(pos)[-1]) {
                    pos[i] <- max(pos[i], pos[i - 1] + gap)
                }
                pos <- pos - mean(pos - y[ord])
                out <- numeric(length(y))
                out[ord] <- pos
                out
            }
        ),
        public = list(
            asSource = function() {
                return(.("Syntax mode for plots is not yet available."))
            }
        )
    )
}
