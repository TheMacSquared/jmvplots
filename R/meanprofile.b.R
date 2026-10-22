#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
meanprofileClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "meanprofileClass",
        inherit = meanprofileBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                vars <- unlist(self$options$vars)
                if (length(vars) < 2) {
                    insertMinVarsNotice(self, length(vars), 2)
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                vars <- unlist(self$options$vars)
                group <- self$options$group

                df <- self$data[vars]
                df[] <- lapply(df, jmvcore::toNumeric)
                df$group <- if (is.null(group)) factor("all") else factor(self$data[[group]])

                # complete cases: every profile is drawn from the same units
                df <- stats::na.omit(df)

                # standardize over the whole sample (not per group), so that
                # the groups stay comparable
                if (self$options$scale == "z") {
                    for (v in vars) {
                        s <- stats::sd(df[[v]])
                        df[[v]] <- if (is.finite(s) && s > 0) (df[[v]] - mean(df[[v]])) / s else 0
                    }
                }

                # one row per group and variable: mean and the half-width of
                # the error bar
                rows <- list()
                for (g in levels(df$group)) {
                    sub <- df[df$group == g, vars, drop = FALSE]
                    n <- nrow(sub)
                    if (n == 0) {
                        next
                    }
                    for (k in seq_along(vars)) {
                        x <- sub[[vars[k]]]
                        se <- if (n > 1) stats::sd(x) / sqrt(n) else NA_real_
                        half <- switch(self$options$errorBars,
                            se = se,
                            ci = if (n > 1) stats::qt(0.975, n - 1) * se else NA_real_,
                            NA_real_
                        )
                        rows[[length(rows) + 1]] <- data.frame(
                            group = g, varIndex = k, mean = mean(x), half = half
                        )
                    }
                }
                state <- do.call(rbind, rows)
                if (!is.null(state)) {
                    state$group <- factor(state$group, levels = levels(df$group))
                }

                image$setState(state)
            },
            .meanprofilePlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state
                vars <- unlist(self$options$vars)
                hasGroup <- !is.null(self$options$group)
                if (nrow(df) == 0) {
                    stop("Profil średnich wymaga kompletnych obserwacji dla co najmniej 2 zmiennych.", call. = FALSE)
                }

                if (hasGroup) {
                    # wrap long legend labels (colours still come from ggtheme)
                    wrapped <- jmvcore::wrapLabels(levels(df$group))
                    if (!anyDuplicated(wrapped)) {
                        levels(df$group) <- wrapped
                    }
                    mapping <- aes(x = varIndex, y = mean, group = group, color = group)
                } else {
                    mapping <- aes(x = varIndex, y = mean, group = group)
                }
                fixed <- if (hasGroup) list() else list(color = theme$color[1])
                dodge <- ggplot2::position_dodge(width = if (hasGroup) 0.15 else 0)

                p <- ggplot(df, mapping)
                if (self$options$scale == "z") {
                    p <- p + ggplot2::geom_hline(yintercept = 0, color = "grey70")
                }
                if (self$options$errorBars != "none") {
                    p <- p + do.call(ggplot2::geom_errorbar, c(list(
                        mapping = aes(ymin = mean - half, ymax = mean + half),
                        width = 0.12, position = dodge, na.rm = TRUE
                    ), fixed))
                }
                p <- p +
                    do.call(ggplot2::geom_line, c(list(
                        linewidth = self$options$lineWidth, position = dodge
                    ), fixed)) +
                    do.call(ggplot2::geom_point, c(list(
                        size = self$options$pointSize, position = dodge
                    ), fixed)) +
                    ggplot2::scale_x_continuous(
                        breaks = seq_along(vars),
                        labels = jmvcore::wrapLabels(vars),
                        expand = ggplot2::expansion(add = 0.3)
                    )

                yLabel <- if (self$options$scale == "z") .("Mean (z)") else .("Mean")
                if (self$options$errorBars == "se") {
                    yLabel <- paste(yLabel, .("± standard error"))
                } else if (self$options$errorBars == "ci") {
                    yLabel <- paste(yLabel, .("± 95% CI"))
                }

                labelDefaults <- list(
                    xLabel = "",
                    yLabel = yLabel,
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
