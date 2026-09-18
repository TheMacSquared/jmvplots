#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
radarClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "radarClass",
        inherit = radarBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                vars <- unlist(self$options$vars)
                if (length(vars) < 3) {
                    insertMinVarsNotice(self, length(vars), 3)
                    return()
                }

                if (self$options$scaling == "pctmax" && is.null(self$options$group)) {
                    # the reference is the largest group mean, so a single
                    # polygon would sit on the outer ring by construction
                    notice <- jmvcore::Notice$new(
                        self$options,
                        name = ".pctmaxGroup",
                        type = jmvcore::NoticeType$INFO,
                        content = .("Scaling by percent of the largest group mean requires a grouping variable.")
                    )
                    self$results$insert(1, notice)
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

                # scale each variable to [0, 1] over the whole data set (not
                # per group); constant variables map to 0.5
                rescale <- function(x, lo, hi) {
                    if (is.finite(lo) && hi > lo) {
                        pmin(pmax((x - lo) / (hi - lo), 0), 1)
                    } else {
                        rep(0.5, length(x))
                    }
                }

                scaling <- self$options$scaling
                for (v in vars) {
                    x <- df[[v]]
                    if (length(x) == 0) {
                        next
                    }
                    if (scaling == "minmax") {
                        df[[v]] <- rescale(x, min(x), max(x))
                    } else if (scaling == "robust") {
                        # values beyond the 5th / 95th percentile are clipped
                        q <- stats::quantile(x, c(0.05, 0.95), names = FALSE)
                        df[[v]] <- rescale(x, q[1], q[2])
                    } else if (scaling == "percentile") {
                        # mid-rank percentile: the overall mean is always 0.5
                        df[[v]] <- (rank(x, ties.method = "average") - 0.5) / length(x)
                    } else if (min(x) < 0) {
                        # pctmax keeps raw values (scaled after aggregation)
                        stop("Skalowanie „procent największej średniej grupowej” wymaga zmiennych bez wartości ujemnych.", call. = FALSE)
                    }
                }

                image$setState(df)
            },
            .radarPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state
                vars <- unlist(self$options$vars)
                K <- length(vars)
                if (nrow(df) == 0 || K < 3) {
                    stop("Wykres radarowy wymaga kompletnych obserwacji dla co najmniej 3 zmiennych.", call. = FALSE)
                }

                hasGroup <- "group" %in% colnames(df)
                if (hasGroup) {
                    # wrap long legend labels (colours still come from ggtheme)
                    wrapped <- jmvcore::wrapLabels(levels(df$group))
                    if (!anyDuplicated(wrapped)) {
                        levels(df$group) <- wrapped
                    }
                }

                # angle of the k-th axis; the first axis points up and the
                # axes proceed clockwise
                axisAngle <- pi / 2 - 2 * pi * (seq_len(K) - 1) / K

                # aggregate: mean of the normalized values per variable
                # (and per group level when a grouping variable is given)
                if (hasGroup) {
                    means <- stats::aggregate(
                        df[vars],
                        by = list(group = df$group),
                        FUN = mean
                    )
                    if (self$options$scaling == "pctmax") {
                        # each axis runs from 0 to the largest group mean
                        for (v in vars) {
                            top <- max(means[[v]])
                            means[[v]] <- if (top > 0) means[[v]] / top else 0
                        }
                    }
                    poly <- do.call(rbind, lapply(seq_len(nrow(means)), function(i) {
                        data.frame(
                            group = means$group[i],
                            varIndex = seq_len(K),
                            r = as.numeric(means[i, vars])
                        )
                    }))
                    poly$group <- factor(poly$group, levels = levels(df$group))
                } else {
                    poly <- data.frame(
                        varIndex = seq_len(K),
                        r = vapply(df[vars], mean, numeric(1))
                    )
                }

                # polygon vertices in cartesian coordinates (coord_polar
                # draws arcs between points, so the polygon is built manually)
                poly$x <- poly$r * cos(axisAngle[poly$varIndex])
                poly$y <- poly$r * sin(axisAngle[poly$varIndex])

                if (hasGroup) {
                    p <- ggplot(poly, aes(x = x, y = y, fill = group, color = group))
                } else {
                    p <- ggplot(poly, aes(x = x, y = y))
                }

                if (self$options$showGrid) {
                    # polygonal grid rings at 25/50/75/100% of the radius
                    ringIdx <- c(seq_len(K), 1) # close each ring
                    rings <- do.call(rbind, lapply(c(0.25, 0.5, 0.75, 1.0), function(r) {
                        data.frame(
                            ring = r,
                            x = r * cos(axisAngle[ringIdx]),
                            y = r * sin(axisAngle[ringIdx])
                        )
                    }))

                    spokes <- data.frame(
                        x = 0,
                        y = 0,
                        xend = cos(axisAngle),
                        yend = sin(axisAngle)
                    )

                    ringLabels <- data.frame(
                        x = 0.03,
                        y = c(0.25, 0.5, 0.75, 1.0),
                        label = c("25%", "50%", "75%", "100%")
                    )

                    p <- p +
                        ggplot2::geom_path(
                            data = rings,
                            mapping = aes(x = x, y = y, group = ring),
                            inherit.aes = FALSE,
                            color = "grey85"
                        ) +
                        ggplot2::geom_segment(
                            data = spokes,
                            mapping = aes(x = x, y = y, xend = xend, yend = yend),
                            inherit.aes = FALSE,
                            color = "grey85"
                        ) +
                        ggplot2::geom_text(
                            data = ringLabels,
                            mapping = aes(x = x, y = y, label = label),
                            inherit.aes = FALSE,
                            hjust = 0,
                            size = 2.8,
                            color = "grey60"
                        )
                }

                if (hasGroup) {
                    p <- p +
                        ggplot2::geom_polygon(
                            alpha = self$options$fillOpacity,
                            linewidth = self$options$lineWidth
                        )
                } else {
                    p <- p +
                        ggplot2::geom_polygon(
                            color = theme$color[1],
                            fill = theme$fill[2],
                            alpha = self$options$fillOpacity,
                            linewidth = self$options$lineWidth
                        )
                }

                # variable labels just outside the outer ring; horizontal
                # justification follows the side of the plot the label is on
                axisLabels <- data.frame(
                    x = 1.18 * cos(axisAngle),
                    y = 1.18 * sin(axisAngle),
                    label = jmvcore::wrapLabels(vars),
                    hjust = ifelse(
                        cos(axisAngle) > 0.1,
                        0,
                        ifelse(cos(axisAngle) < -0.1, 1, 0.5)
                    )
                )

                p <- p +
                    ggplot2::geom_text(
                        data = axisLabels,
                        mapping = aes(x = x, y = y, label = label, hjust = hjust),
                        inherit.aes = FALSE,
                        size = 3.5
                    ) +
                    ggplot2::coord_fixed(
                        xlim = c(-1.45, 1.45),
                        ylim = c(-1.35, 1.35)
                    )

                labelDefaults <- list(
                    title = NULL,
                    caption = switch(
                        self$options$scaling,
                        minmax = .("Axes show min–max normalized values"),
                        robust = .("Axes: 0% = 5th percentile, 100% = 95th percentile of the observations"),
                        pctmax = .("Axes: 100% = the largest group mean"),
                        percentile = .("Axes show mean percentile ranks")
                    ),
                    xLabel = NULL,
                    yLabel = NULL,
                    groupLabel = self$options$group
                )

                p <- p +
                    ggtheme +
                    formatLegend(self$options) +
                    setLabels(
                        options = private$.labelOptions(),
                        defaults = labelDefaults
                    ) +
                    formatLabels(options = private$.labelOptions()) +
                    # hide axes; the .x/.y variants must be blanked explicitly
                    # because formatLabels() sets them and explicit child
                    # elements win over a blanked parent element
                    ggplot2::theme(
                        axis.text = ggplot2::element_blank(),
                        axis.text.x = ggplot2::element_blank(),
                        axis.text.y = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank(),
                        axis.title = ggplot2::element_blank(),
                        axis.title.x = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank(),
                        panel.grid = ggplot2::element_blank(),
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.major.y = ggplot2::element_blank(),
                        panel.grid.minor.x = ggplot2::element_blank(),
                        panel.grid.minor.y = ggplot2::element_blank()
                    )

                return(p)
            },
            #### Helper functions ----
            # setLabels()/formatLabels() expect axis label options that this
            # analysis does not define (the plot has no axes), so pad the
            # options with inert axis values; the axis elements are blanked
            # out afterwards anyway
            .labelOptions = function() {
                return(list(
                    title = self$options$title,
                    titleAlign = self$options$titleAlign,
                    titleFontSize = self$options$titleFontSize,
                    titleFontFace = self$options$titleFontFace,
                    subtitle = self$options$subtitle,
                    subtitleAlign = self$options$subtitleAlign,
                    subtitleFontSize = self$options$subtitleFontSize,
                    subtitleFontFace = self$options$subtitleFontFace,
                    caption = self$options$caption,
                    captionAlign = self$options$captionAlign,
                    captionFontSize = self$options$captionFontSize,
                    captionFontFace = self$options$captionFontFace,
                    xLabel = "",
                    xLabelAlign = "center",
                    xLabelFontSize = 16,
                    xLabelFontFace = "plain",
                    yLabel = "",
                    yLabelAlign = "center",
                    yLabelFontSize = 16,
                    yLabelFontFace = "plain",
                    xAxisLabelFontSize = 12,
                    xAxisLabelRotation = 0,
                    yAxisLabelFontSize = 12,
                    yAxisLabelRotation = 0,
                    legendTitle = self$options$legendTitle,
                    legendTitleFontSize = self$options$legendTitleFontSize,
                    legendTitleFontFace = self$options$legendTitleFontFace,
                    legendLabelFontSize = self$options$legendLabelFontSize,
                    legendLabelFontFace = self$options$legendLabelFontFace
                ))
            }
        ),
        public = list(
            asSource = function() {
                return(.("Syntax mode for plots is not yet available."))
            }
        )
    )
}
