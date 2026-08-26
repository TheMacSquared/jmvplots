#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
parcoordClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "parcoordClass",
        inherit = parcoordBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                vars <- unlist(self$options$vars)
                if (length(vars) < 2) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                vars <- unlist(self$options$vars)
                group <- self$options$group

                if (is.null(group)) {
                    df <- self$data |>
                        dplyr::select(dplyr::all_of(vars))
                } else {
                    df <- self$data |>
                        dplyr::select(
                            dplyr::all_of(vars),
                            group = !!sym(group)
                        ) |>
                        dplyr::mutate(group = factor(group))
                }

                df <- df |>
                    dplyr::mutate(
                        dplyr::across(dplyr::all_of(vars), jmvcore::toNumeric)
                    )

                # drop rows with any missing value
                df <- stats::na.omit(df)

                # min-max normalize each variable to [0, 1] over the whole
                # data set (not per group); constant variables map to 0.5
                for (v in vars) {
                    rng <- range(df[[v]])
                    if (is.finite(rng[1]) && rng[2] > rng[1]) {
                        df[[v]] <- (df[[v]] - rng[1]) / (rng[2] - rng[1])
                    } else {
                        df[[v]] <- rep(0.5, nrow(df))
                    }
                }

                # long format: one row per observation and variable
                df$id <- seq_len(nrow(df))
                long <- do.call(rbind, lapply(seq_along(vars), function(k) {
                    part <- data.frame(
                        id = df$id,
                        varIndex = k,
                        value = df[[vars[k]]]
                    )
                    if (!is.null(group)) {
                        part$group <- df$group
                    }
                    part
                }))

                image$setState(long)
            },
            .parcoordPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state
                vars <- unlist(self$options$vars)
                K <- length(vars)
                if (nrow(df) == 0 || K < 2) {
                    return(FALSE)
                }

                hasGroup <- "group" %in% colnames(df)
                if (hasGroup) {
                    # wrap long legend labels (colours still come from ggtheme)
                    wrapped <- jmvcore::wrapLabels(levels(df$group))
                    if (!anyDuplicated(wrapped)) {
                        levels(df$group) <- wrapped
                    }
                }

                axes <- data.frame(x = seq_len(K))

                if (hasGroup) {
                    p <- ggplot(
                        df,
                        aes(x = varIndex, y = value, group = id, color = group)
                    ) +
                        ggplot2::geom_segment(
                            data = axes,
                            mapping = aes(x = x, xend = x, y = 0, yend = 1),
                            inherit.aes = FALSE,
                            color = "grey80",
                            linewidth = 0.5
                        ) +
                        ggplot2::geom_line(
                            alpha = self$options$lineOpacity,
                            linewidth = self$options$lineWidth
                        )
                } else {
                    p <- ggplot(
                        df,
                        aes(x = varIndex, y = value, group = id)
                    ) +
                        ggplot2::geom_segment(
                            data = axes,
                            mapping = aes(x = x, xend = x, y = 0, yend = 1),
                            inherit.aes = FALSE,
                            color = "grey80",
                            linewidth = 0.5
                        ) +
                        ggplot2::geom_line(
                            alpha = self$options$lineOpacity,
                            linewidth = self$options$lineWidth,
                            color = theme$color[1]
                        )
                }

                p <- p +
                    ggplot2::scale_x_continuous(
                        breaks = seq_len(K),
                        labels = jmvcore::wrapLabels(vars),
                        expand = ggplot2::expansion(add = 0.3)
                    ) +
                    ggplot2::coord_cartesian(ylim = c(0, 1))

                labelDefaults <- list(
                    xLabel = "",
                    yLabel = .("Normalized value (min–max)"),
                    groupLabel = self$options$group
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
