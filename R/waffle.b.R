#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
waffleClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "waffleClass",
        inherit = waffleBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$group)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                group <- self$options$group
                counts <- self$options$counts
                group2 <- self$options$group2

                if (is.null(group2)) {
                    if (is.null(counts)) {
                        df <- self$data |>
                            dplyr::select(group = !!sym(group)) |>
                            dplyr::filter(!is.na(group)) |>
                            dplyr::mutate(group = factor(group)) |>
                            dplyr::count(group, name = "n", .drop = FALSE)
                    } else {
                        df <- self$data |>
                            dplyr::select(group = !!sym(group), n = !!sym(counts)) |>
                            dplyr::mutate(n = jmvcore::toNumeric(n)) |>
                            dplyr::filter(!is.na(group), !is.na(n)) |>
                            dplyr::mutate(group = factor(group)) |>
                            dplyr::group_by(group) |>
                            dplyr::summarize(n = sum(n), .groups = "drop")
                    }
                } else {
                    # one waffle panel per level of the grouping variable
                    if (is.null(counts)) {
                        df <- self$data |>
                            dplyr::select(
                                group = !!sym(group),
                                facet = !!sym(group2)
                            ) |>
                            dplyr::filter(!is.na(group), !is.na(facet)) |>
                            dplyr::mutate(
                                group = factor(group),
                                facet = factor(facet)
                            ) |>
                            dplyr::count(facet, group, name = "n", .drop = FALSE)
                    } else {
                        df <- self$data |>
                            dplyr::select(
                                group = !!sym(group),
                                facet = !!sym(group2),
                                n = !!sym(counts)
                            ) |>
                            dplyr::mutate(n = jmvcore::toNumeric(n)) |>
                            dplyr::filter(!is.na(group), !is.na(facet), !is.na(n)) |>
                            dplyr::mutate(
                                group = factor(group),
                                facet = factor(facet)
                            ) |>
                            dplyr::group_by(facet, group) |>
                            dplyr::summarize(n = sum(n), .groups = "drop")
                    }
                }

                # guard against negative weights
                df <- df |> dplyr::filter(n >= 0)

                image$setState(df)
            },
            .wafflePlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state
                total <- sum(df$n)
                if (nrow(df) == 0 || total <= 0) {
                    return(FALSE)
                }

                # wrap long legend labels (colours still come from ggtheme)
                wrapped <- jmvcore::wrapLabels(levels(df$group))
                if (!anyDuplicated(wrapped)) {
                    levels(df$group) <- wrapped
                }

                labelsPosition <- self$options$percentLabelsPosition
                if (self$options$percentLabels && labelsPosition == "legend") {
                    # percentages appended to the legend labels; these are
                    # shares of the grand total (a shared legend cannot show
                    # per-panel percentages when a grouping variable is set)
                    totals <- tapply(df$n, df$group, sum)
                    totals[is.na(totals)] <- 0
                    prop <- as.numeric(totals) / sum(totals)
                    levels(df$group) <- paste0(
                        levels(df$group),
                        " (", scales::percent(prop, accuracy = 1), ")"
                    )
                }

                hasFacet <- "facet" %in% colnames(df)
                if (hasFacet) {
                    wrappedFacet <- jmvcore::wrapLabels(levels(df$facet))
                    if (!anyDuplicated(wrappedFacet)) {
                        levels(df$facet) <- wrappedFacet
                    }
                }

                # allocate exactly 100 squares with the largest remainder method
                allocateSquares <- function(n) {
                    exact <- n / sum(n) * 100
                    squares <- floor(exact)
                    remainder <- exact - squares
                    shortfall <- 100 - sum(squares)
                    if (shortfall > 0) {
                        topUp <- order(remainder, decreasing = TRUE)[seq_len(shortfall)]
                        squares[topUp] <- squares[topUp] + 1
                    }
                    squares
                }

                # levels with 0 squares are omitted from the grid but kept
                # in the factor levels so they still appear in the legend;
                # squares fill the grid column by column
                rows <- self$options$rows
                buildTiles <- function(groups, n) {
                    tileGroup <- rep(as.character(groups), times = allocateSquares(n))
                    idx <- seq_along(tileGroup)
                    data.frame(
                        x = (idx - 1) %/% rows,
                        y = (idx - 1) %% rows,
                        group = tileGroup
                    )
                }

                if (hasFacet) {
                    # each panel is its own 100%: one square = 1% of that group
                    parts <- lapply(split(df, df$facet), function(d) {
                        if (sum(d$n) <= 0)
                            return(NULL)
                        part <- buildTiles(d$group, d$n)
                        part$facet <- d$facet[1]
                        part
                    })
                    tiles <- do.call(rbind, parts)
                    if (is.null(tiles) || nrow(tiles) == 0) {
                        return(FALSE)
                    }
                    tiles$facet <- factor(tiles$facet, levels = levels(df$facet))
                } else {
                    tiles <- buildTiles(df$group, df$n)
                }
                tiles$group <- factor(tiles$group, levels = levels(df$group))

                labels <- NULL
                if (self$options$percentLabels && labelsPosition != "legend") {
                    # percentages come from the true proportions, not from
                    # the rounded square counts; positions from the centroid
                    # of each category's squares
                    computeLabels <- function(tilesPart, d) {
                        agg <- stats::aggregate(
                            cbind(x, y) ~ group,
                            data = tilesPart,
                            FUN = mean
                        )
                        prop <- d$n / sum(d$n)
                        agg$prop <- prop[match(agg$group, as.character(d$group))]
                        agg
                    }

                    if (hasFacet) {
                        labelParts <- lapply(levels(df$facet), function(f) {
                            tilesPart <- tiles[tiles$facet == f, ]
                            if (nrow(tilesPart) == 0)
                                return(NULL)
                            lab <- computeLabels(tilesPart, df[df$facet == f, ])
                            lab$facet <- f
                            lab
                        })
                        labels <- do.call(rbind, labelParts)
                        labels$facet <- factor(labels$facet, levels = levels(df$facet))
                    } else {
                        labels <- computeLabels(tiles, df)
                    }
                    labels$label <- scales::percent(labels$prop, accuracy = 1)

                    # outside: above the grid, over each category's columns
                    if (self$options$percentLabelsPosition == "outside") {
                        labels$y <- rows + 0.15
                    }
                }

                if (hasFacet) {
                    captionDefault <- .("Each square = 1% of the group's total")
                } else {
                    captionDefault <- .("Each square = 1% of the total")
                }

                labelDefaults <- list(
                    title = NULL,
                    caption = captionDefault,
                    xLabel = NULL,
                    yLabel = NULL,
                    groupLabel = self$options$group
                )

                p <- ggplot(tiles, aes(x = x, y = y, fill = group)) +
                    ggplot2::geom_tile(
                        color = "white",
                        linewidth = 1.5,
                        width = 0.9,
                        height = 0.9
                    ) +
                    ggplot2::coord_equal()

                if (hasFacet) {
                    p <- p + ggplot2::facet_wrap(~ facet)
                }

                if (!is.null(labels)) {
                    if (self$options$percentLabelsPosition == "inside") {
                        # a translucent backing keeps the label readable on
                        # light and dark tiles alike
                        p <- p +
                            ggplot2::geom_label(
                                data = labels,
                                mapping = aes(x = x, y = y, label = label),
                                inherit.aes = FALSE,
                                color = "grey20",
                                fill = "white",
                                alpha = 0.75,
                                label.size = 0,
                                size = 3.4
                            )
                    } else {
                        p <- p +
                            ggplot2::geom_text(
                                data = labels,
                                mapping = aes(x = x, y = y, label = label),
                                inherit.aes = FALSE,
                                size = 3.6
                            )
                    }
                }

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
