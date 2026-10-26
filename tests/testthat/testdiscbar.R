#' Discrete bar plot (jUPWR): one bar per integer value

discbarGgplot <- function(res) {
    analysis <- res$.__enclos_env__$private$.parent
    priv <- analysis$.__enclos_env__$private
    opts <- analysis$options
    gt <- utils::getFromNamespace("getGlobalTheme", "jmvcore")(opts$theme, opts$palette)
    priv$.discbarPlot(res$plot, gt$ggtheme, gt$theme)
}

discbarData <- data.frame(
    k = c(0, 1, 1, 2, 2, 2, 5, NA, 1, 2),
    g = factor(c("A", "A", "B", "A", "B", "B", "A", "B", NA, "A"))
)

testthat::test_that("discbar: every integer in the range, absent values with zero counts", {
    res <- scatr::discbar(data = discbarData, var = "k", group = NULL)
    state <- res$plot$state

    testthat::expect_equal(state$x, 0:5)
    testthat::expect_equal(state$n, c(1, 3, 4, 0, 0, 1))
    testthat::expect_equal(sum(state$prop), 1)

    b <- ggplot2::ggplot_build(discbarGgplot(res))
    testthat::expect_equal(b$layout$panel_params[[1]]$x$get_labels(), as.character(0:5))
})

testthat::test_that("discbar: proportions within each group, stems and panels render", {
    res <- scatr::discbar(
        data = discbarData, var = "k", group = "g", yType = "prop", geom = "stems"
    )
    state <- res$plot$state

    testthat::expect_equal(as.numeric(tapply(state$prop, state$group, sum)), c(1, 1))
    testthat::expect_equal(state$n[state$group == "A" & state$x == 2], 2)
    testthat::expect_s3_class(discbarGgplot(res), "ggplot")

    res <- scatr::discbar(data = discbarData, var = "k", group = "g", facet = TRUE)
    testthat::expect_s3_class(discbarGgplot(res), "ggplot")
})

testthat::test_that("discbar: non-integer values are rejected", {
    testthat::expect_error(
        scatr::discbar(data = data.frame(x = c(1, 2.5, 3)), var = "x", group = NULL),
        "niecałkowite"
    )
})
