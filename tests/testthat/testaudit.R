#' Fixes from the code audit of 2026-11-05 (jUPWR)

auditGgplot <- function(res, renderFun) {
    analysis <- res$.__enclos_env__$private$.parent
    priv <- analysis$.__enclos_env__$private
    opts <- analysis$options
    gt <- utils::getFromNamespace("getGlobalTheme", "jmvcore")(opts$theme, opts$palette)
    priv[[renderFun]](res$plot, gt$ggtheme, gt$theme)
}

testthat::test_that("raincloud: a missing group value does not break the plot", {
    d <- data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 8), g = c("a", "a", "a", NA, "b", "b", "b", "b"))
    res <- scatr::raincloud(data = d, var = "y", group = "g")
    p <- auditGgplot(res, ".raincloudPlot")
    testthat::expect_s3_class(p, "ggplot")
    testthat::expect_silent(ggplot2::ggplot_build(p))
})

testthat::test_that("stripmean and raincloud: 'exclude cases listwise' works in wide mode", {
    d <- data.frame(a = c(1, 2, 3, 100, 100), b = c(1, 2, 3, NA, NA))
    for (fn in list(scatr::stripmean, scatr::raincloud)) {
        st <- fn(data = d, mode = "wide", vars = c("a", "b"), naOmit = TRUE)$plot$state
        testthat::expect_equal(as.integer(table(st$x)), c(3, 3))
        st2 <- fn(data = d, mode = "wide", vars = c("a", "b"), naOmit = FALSE)$plot$state
        testthat::expect_equal(as.integer(table(st2$x)), c(5, 3))
    }
})

testthat::test_that("stripmean swarm: points stay on their category when a column is empty", {
    d <- data.frame(a = c(1, 2, 3, 4), b = NA_real_, c = c(5, 6, 7, 8))
    res <- scatr::stripmean(data = d, mode = "wide", vars = c("a", "b", "c"), pointLayout = "swarm")
    b <- ggplot2::ggplot_build(auditGgplot(res, ".stripPlot"))
    pts <- b$data[[2]]
    # two categories on the axis (a, c): points of c around 2, not 3
    testthat::expect_lt(max(pts$x), 2.5)
})

testthat::test_that("bubble: non-positive sizes are left out with a notice", {
    d <- data.frame(x = 1:6, y = 1:6, s = c(-5, -1, 0, 1, 5, 10))
    res <- scatr::bubble(data = d, x = "x", y = "y", size = "s")
    testthat::expect_equal(res$plot$state$size, c(1, 5, 10))
    names <- vapply(res$items, function(i) i$name, "")
    testthat::expect_true(".nonPositiveSize" %in% names)
})
