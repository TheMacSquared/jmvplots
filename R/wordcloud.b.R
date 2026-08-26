#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang sym
#' @importFrom jmvcore .
wordcloudClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "wordcloudClass",
        inherit = wordcloudBase,
        private = list(
            .init = function() {
                image <- self$results$plot
            },
            .run = function() {
                if (is.null(self$options$words)) {
                    return()
                }

                private$.preparePlotData()
            },
            #### Plot functions ----
            .preparePlotData = function() {
                image <- self$results$plot

                words <- self$options$words
                counts <- self$options$counts

                if (is.null(counts)) {
                    # no counts variable: word frequencies are the level counts
                    df <- self$data |>
                        dplyr::select(word = !!sym(words)) |>
                        dplyr::filter(!is.na(word)) |>
                        dplyr::mutate(word = as.character(word)) |>
                        dplyr::count(word, name = "freq")
                } else {
                    df <- self$data |>
                        dplyr::select(word = !!sym(words), freq = !!sym(counts)) |>
                        dplyr::mutate(freq = jmvcore::toNumeric(freq)) |>
                        dplyr::filter(!is.na(word), !is.na(freq)) |>
                        dplyr::mutate(word = as.character(word)) |>
                        dplyr::group_by(word) |>
                        dplyr::summarize(freq = sum(freq), .groups = "drop")
                }

                # keep positive frequencies only, sorted decreasing so the
                # biggest word is placed first, truncated to the word limit
                df <- df |>
                    dplyr::filter(freq > 0) |>
                    dplyr::arrange(dplyr::desc(freq)) |>
                    dplyr::slice_head(n = self$options$maxWords)

                image$setState(df)
            },
            .wordcloudPlot = function(image, ggtheme, theme, ...) {
                if (is.null(image$state)) {
                    return(FALSE)
                }

                df <- image$state
                if (nrow(df) == 0) {
                    return(FALSE)
                }

                minFontSize <- self$options$minFontSize
                maxFontSize <- self$options$maxFontSize

                # font size in points; sqrt so that the label area is roughly
                # proportional to the word's frequency
                sizes <- minFontSize +
                    (maxFontSize - minFontSize) * sqrt(df$freq / max(df$freq))

                placed <- private$.placeWords(df$word, sizes)
                if (nrow(placed) == 0) {
                    return(FALSE)
                }

                # colours are purely decorative (no legend); the palette
                # follows the plot theme
                placed$word <- factor(placed$word, levels = placed$word)
                pal <- jmvcore::colorPalette(
                    nlevels(placed$word), theme$palette, 'color'
                )

                # geom_text sizes are in mm; convert from points
                placed$size <- placed$size / ggplot2::.pt

                # panel limits from the bounding box of the placed words,
                # padded by 5% on each side
                xRange <- range(
                    placed$x - placed$w / 2,
                    placed$x + placed$w / 2
                )
                yRange <- range(
                    placed$y - placed$h / 2,
                    placed$y + placed$h / 2
                )
                xPad <- 0.05 * diff(xRange)
                yPad <- 0.05 * diff(yRange)

                labelDefaults <- list(
                    xLabel = NULL,
                    yLabel = NULL,
                    groupLabel = NULL
                )

                p <- ggplot(
                    placed,
                    aes(x = x, y = y, label = word, color = word, size = size)
                ) +
                    ggplot2::geom_text() +
                    ggplot2::scale_size_identity() +
                    ggplot2::coord_fixed(
                        xlim = c(xRange[1] - xPad, xRange[2] + xPad),
                        ylim = c(yRange[1] - yPad, yRange[2] + yPad),
                        expand = FALSE
                    ) +
                    ggtheme +
                    # the manual colour scale must come AFTER ggtheme, whose
                    # own discrete colour scale would override it (and bring
                    # its legend back)
                    ggplot2::scale_color_manual(values = pal, guide = "none") +
                    setLabels(
                        options = private$.labelOptions(),
                        defaults = labelDefaults,
                        legend = FALSE
                    ) +
                    formatLabels(
                        options = private$.labelOptions(),
                        legend = FALSE
                    ) +
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
            # Word cloud layout in plain R. Each word is modelled as an
            # axis-aligned rectangle in plot units; the biggest word sits at
            # the origin and every following word walks outwards along a
            # slightly flattened Archimedean spiral until it finds the first
            # position free of overlaps with the already placed rectangles.
            # Words that find no free spot within the step limit are skipped.
            #
            # @param words A character vector, sorted by decreasing frequency.
            # @param sizes The font sizes in points, one per word.
            # @return A data.frame with columns x, y, word, size, w, h.
            .placeWords = function(words, sizes) {
                # rectangle model of each word; 0.011 approximates the
                # point -> plot-unit conversion for a panel spanning [-1, 1]
                w <- 0.011 * sizes * (nchar(words) + 1) * 0.55
                h <- 0.011 * sizes * 1.25
                margin <- 0.004
                maxSteps <- 4000

                px <- numeric(0)
                py <- numeric(0)
                pw <- numeric(0)
                ph <- numeric(0)
                keep <- integer(0)

                for (i in seq_along(words)) {
                    theta <- 0
                    for (step in seq_len(maxSteps)) {
                        r <- 0.006 * theta
                        x <- r * cos(theta) * 1.4
                        y <- r * sin(theta) * 0.9

                        # AABB overlap test against all placed rectangles,
                        # with a small margin between neighbouring words
                        collides <- any(
                            abs(x - px) < (w[i] + pw) / 2 + margin &
                            abs(y - py) < (h[i] + ph) / 2 + margin
                        )
                        if (!collides) {
                            px <- c(px, x)
                            py <- c(py, y)
                            pw <- c(pw, w[i])
                            ph <- c(ph, h[i])
                            keep <- c(keep, i)
                            break
                        }
                        theta <- theta + 0.4
                    }
                }

                return(data.frame(
                    x = px,
                    y = py,
                    word = words[keep],
                    size = sizes[keep],
                    w = pw,
                    h = ph,
                    stringsAsFactors = FALSE
                ))
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
                    yAxisLabelRotation = 0
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
