#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
treemapClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "treemapClass",
        inherit = treemapBase,
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
                size <- self$options$size
                group2 <- self$options$group2

                if (is.null(group2)) {
                    if (is.null(size)) {
                        # no size variable: rectangle sizes are the level counts
                        df <- self$data |>
                            dplyr::select(group = !!sym(group)) |>
                            dplyr::filter(!is.na(group)) |>
                            dplyr::mutate(group = factor(group)) |>
                            dplyr::count(group, name = "size")
                    } else {
                        df <- self$data |>
                            dplyr::select(group = !!sym(group), size = !!sym(size)) |>
                            dplyr::mutate(size = jmvcore::toNumeric(size)) |>
                            dplyr::filter(!is.na(group), !is.na(size)) |>
                            dplyr::mutate(group = factor(group)) |>
                            dplyr::group_by(group) |>
                            dplyr::summarize(size = sum(size), .groups = "drop")
                    }

                    # keep positive sizes only, sorted decreasing for squarify
                    df <- df |>
                        dplyr::filter(size > 0) |>
                        dplyr::arrange(dplyr::desc(size))
                } else {
                    # nested treemap: outer blocks are the grouping variable
                    if (is.null(size)) {
                        df <- self$data |>
                            dplyr::select(
                                group = !!sym(group),
                                block = !!sym(group2)
                            ) |>
                            dplyr::filter(!is.na(group), !is.na(block)) |>
                            dplyr::mutate(
                                group = factor(group),
                                block = factor(block)
                            ) |>
                            dplyr::count(block, group, name = "size")
                    } else {
                        df <- self$data |>
                            dplyr::select(
                                group = !!sym(group),
                                block = !!sym(group2),
                                size = !!sym(size)
                            ) |>
                            dplyr::mutate(size = jmvcore::toNumeric(size)) |>
                            dplyr::filter(
                                !is.na(group), !is.na(block), !is.na(size)
                            ) |>
                            dplyr::mutate(
                                group = factor(group),
                                block = factor(block)
                            ) |>
                            dplyr::group_by(block, group) |>
                            dplyr::summarize(size = sum(size), .groups = "drop")
                    }

                    # keep positive sizes only, items sorted decreasing
                    # within each block for squarify
                    df <- df |>
                        dplyr::filter(size > 0) |>
                        dplyr::arrange(block, dplyr::desc(size))
                }

                image$setState(df)
            },
            .treemapPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state
                if (nrow(df) == 0) {
                    return(FALSE)
                }

                # wrap long legend labels (colours still come from ggtheme)
                wrapped <- jmvcore::wrapLabels(levels(df$group))
                if (!anyDuplicated(wrapped)) {
                    levels(df$group) <- wrapped
                }

                hasBlocks <- "block" %in% colnames(df)

                if (hasBlocks) {
                    # outer rectangles sized by block totals, then each
                    # block's items squarified inside its own rectangle
                    blocks <- df |>
                        dplyr::group_by(block) |>
                        dplyr::summarize(size = sum(size), .groups = "drop") |>
                        dplyr::arrange(dplyr::desc(size))
                    outer <- private$.squarify(blocks$size)
                    outer$block <- blocks$block

                    rectList <- lapply(seq_len(nrow(outer)), function(i) {
                        sub <- df[df$block == outer$block[i], ]
                        r <- private$.squarify(
                            sub$size,
                            x = outer$xmin[i],
                            y = outer$ymin[i],
                            w = outer$xmax[i] - outer$xmin[i],
                            h = outer$ymax[i] - outer$ymin[i]
                        )
                        r$group <- sub$group
                        r$value <- sub$size
                        r
                    })
                    rects <- do.call(rbind, rectList)
                    rects$prop <- rects$value / sum(rects$value)
                } else {
                    rects <- private$.squarify(df$size)
                    rects$group <- df$group
                    rects$value <- df$size
                    rects$prop <- df$size / sum(df$size)
                }

                p <- ggplot(
                    rects,
                    aes(
                        xmin = xmin,
                        xmax = xmax,
                        ymin = ymin,
                        ymax = ymax,
                        fill = group
                    )
                ) +
                    ggplot2::geom_rect(
                        color = "white",
                        linewidth = if (hasBlocks) 0.8 else 1.5
                    ) +
                    ggplot2::coord_cartesian(expand = FALSE)

                if (hasBlocks) {
                    # thick borders and a bold corner label mark the blocks
                    p <- p +
                        ggplot2::geom_rect(
                            data = outer,
                            mapping = aes(
                                xmin = xmin,
                                xmax = xmax,
                                ymin = ymin,
                                ymax = ymax
                            ),
                            inherit.aes = FALSE,
                            fill = NA,
                            color = "white",
                            linewidth = 2.5
                        ) +
                        ggplot2::geom_text(
                            data = outer,
                            mapping = aes(
                                x = xmin + 0.012,
                                y = ymax - 0.015,
                                label = as.character(block)
                            ),
                            inherit.aes = FALSE,
                            hjust = 0,
                            vjust = 1,
                            fontface = "bold",
                            size = self$options$labelFontSize / ggplot2::.pt,
                            color = "white"
                        )
                }

                if (self$options$showLabels) {
                    labelData <- rects
                    labelData$area <-
                        (labelData$xmax - labelData$xmin) *
                        (labelData$ymax - labelData$ymin)

                    # skip labels on rectangles too small to hold them
                    labelData <- labelData[labelData$area >= 0.02, ]

                    if (nrow(labelData) > 0) {
                        labelData$label <- as.character(labelData$group)

                        if (self$options$showValues) {
                            labelData$label <- paste0(
                                labelData$label,
                                "\n",
                                format(
                                    round(labelData$value, 2),
                                    trim = TRUE,
                                    scientific = FALSE
                                ),
                                " (",
                                scales::percent(labelData$prop, accuracy = 1),
                                ")"
                            )
                        }

                        p <- p +
                            ggplot2::geom_text(
                                mapping = aes(
                                    x = (xmin + xmax) / 2,
                                    y = (ymin + ymax) / 2,
                                    label = label
                                ),
                                data = labelData,
                                inherit.aes = FALSE,
                                size = self$options$labelFontSize / ggplot2::.pt,
                                color = "white"
                            )
                    }
                }

                labelDefaults <- list(
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
            # Squarified treemap layout (Bruls, Huizing & van Wijk, 2000),
            # implemented in plain R. Rows are laid out along the shorter
            # side of the remaining rectangle; items are added to a row as
            # long as the worst aspect ratio does not get worse, then the
            # row is frozen and the remainder is processed.
            #
            # @param sizes A numeric vector of positive areas (any scale).
            # @param x,y,w,h The rectangle to fill (defaults to unit square).
            # @return A data.frame with columns xmin, xmax, ymin, ymax.
            .squarify = function(sizes, x = 0, y = 0, w = 1, h = 1) {
                # normalize sizes so that the total area equals the rectangle area
                sizes <- sizes * (w * h) / sum(sizes)
                n <- length(sizes)
                out <- data.frame(
                    xmin = numeric(n),
                    xmax = numeric(n),
                    ymin = numeric(n),
                    ymax = numeric(n)
                )

                # worst aspect ratio of a row laid out along a side of given length
                worst <- function(row, side) {
                    s <- sum(row)
                    max(vapply(
                        row,
                        function(r) max((side^2 * r) / s^2, s^2 / (side^2 * r)),
                        numeric(1)
                    ))
                }

                i <- 1
                while (i <= n) {
                    side <- min(w, h)
                    row <- sizes[i]
                    j <- i

                    # grow the row while the worst aspect ratio does not get worse
                    while (j < n) {
                        candidate <- c(row, sizes[j + 1])
                        if (worst(candidate, side) <= worst(row, side)) {
                            row <- candidate
                            j <- j + 1
                        } else {
                            break
                        }
                    }

                    # freeze the row and lay it out along the shorter side
                    s <- sum(row)
                    if (w >= h) {
                        # vertical strip on the left, filled bottom to top
                        stripWidth <- s / h
                        yy <- y
                        for (k in seq_along(row)) {
                            rectHeight <- row[k] / stripWidth
                            out[i + k - 1, ] <- c(x, x + stripWidth, yy, yy + rectHeight)
                            yy <- yy + rectHeight
                        }
                        x <- x + stripWidth
                        w <- w - stripWidth
                    } else {
                        # horizontal strip at the bottom, filled left to right
                        stripHeight <- s / w
                        xx <- x
                        for (k in seq_along(row)) {
                            rectWidth <- row[k] / stripHeight
                            out[i + k - 1, ] <- c(xx, xx + rectWidth, y, y + stripHeight)
                            xx <- xx + rectWidth
                        }
                        y <- y + stripHeight
                        h <- h - stripHeight
                    }

                    i <- j + 1
                }

                return(out)
            },
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
