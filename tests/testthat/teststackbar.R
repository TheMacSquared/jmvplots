#' Composition bar plot (jUPWR): X alone, without a segments variable

stackbarGgplot <- function(res) {
    analysis <- res$.__enclos_env__$private$.parent
    priv <- analysis$.__enclos_env__$private
    opts <- analysis$options
    gt <- utils::getFromNamespace("getGlobalTheme", "jmvcore")(opts$theme, opts$palette)
    priv$.stackbarPlot(res$plot, gt$ggtheme, gt$theme)
}

stackbarData <- data.frame(
    k = c("a", "a", "a", "b", "c", "c", NA),
    g = c("x", "y", "x", "y", "x", "y", "x")
)

testthat::test_that("stackbar: X alone, stacked = one bar split by X", {
    res <- scatr::stackbar(data = stackbarData, x = "k", barType = "percent")
    testthat::expect_equal(nlevels(res$plot$state$x), 1)
    b <- ggplot2::layer_data(stackbarGgplot(res))
    testthat::expect_equal(nrow(b), 3)
    testthat::expect_equal(sort(b$ymax - b$ymin), c(1, 2, 3) / 6)
})

testthat::test_that("stackbar: X alone, side by side = one bar per X, shares of the total", {
    for (type in c("count", "percentDodge")) {
        res <- scatr::stackbar(data = stackbarData, x = "k", barType = type, percentLabels = TRUE)
        p <- stackbarGgplot(res)
        b <- ggplot2::layer_data(p)
        expected <- c(3, 1, 2)
        if (type == "percentDodge") expected <- expected / 6
        testthat::expect_equal(b$y, expected)
        testthat::expect_equal(as.numeric(b$x), c(1, 2, 3))
        testthat::expect_equal(p$theme$legend.position, "none")
    }
})

testthat::test_that("stackbar: with segments unchanged (shares within X)", {
    res <- scatr::stackbar(data = stackbarData, x = "k", segments = "g", barType = "percentDodge")
    b <- ggplot2::layer_data(stackbarGgplot(res))
    testthat::expect_equal(nrow(b), 6)
    testthat::expect_equal(sum(b$y[as.numeric(b$x) < 1.5]), 1)
})
