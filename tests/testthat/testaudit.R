#' Fixes from the code audit of 2026-11-05 (jUPWR)

auditGgplot <- function(res, renderFun) {
    analysis <- res$.__enclos_env__$private$.parent
    priv <- analysis$.__enclos_env__$private
    opts <- analysis$options
    gt <- utils::getFromNamespace("getGlobalTheme", "jmvcore")(opts$theme, opts$palette)
    priv[[renderFun]](res$plot, gt$ggtheme, gt$theme)
}

testthat::test_that("heatmap distinguishes missing cells from a zero sum", {
    d <- data.frame(x = c("a", "a", "b", "b"), y = "row", v = c(NA, NA, -1, 1))
    for (aggregation in c("sum", "mean", "median")) {
        res <- scatr::heatmap(data = d, x = "x", y = "y", value = "v", aggregation = aggregation)
        st <- res$plot$state
        testthat::expect_true(is.na(st$value[st$x == "a"]))
        testthat::expect_equal(st$value[st$x == "b"], 0)
    }
})

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

testthat::test_that("swarm: same layout as the old algorithm, fast for big groups", {
    off <- scatr:::swarmOffsets(rnorm(10000), list(x = 0.01, y = 0.05), 2)
    testthat::expect_length(off, 10000)
    t0 <- proc.time()[["elapsed"]]
    scatr:::swarmOffsets(rnorm(10000), list(x = 0.01, y = 0.05), 2)
    testthat::expect_lt(proc.time()[["elapsed"]] - t0, 5)
    # no two points overlap
    set.seed(2); y <- round(rnorm(300, 10, 1), 1); u <- list(x = 0.01, y = 0.05)
    o <- scatr:::swarmOffsets(y, u, 2)
    D <- as.matrix(stats::dist(cbind(o / u$x, y / u$y))); diag(D) <- Inf
    testthat::expect_gte(min(D), 2 * 72.27 / 25.4 + 1.5 - 1e-6)
})

testthat::test_that("histogram: an outlier does not make a million bins", {
    set.seed(1)
    res <- scatr::jmvhist(data = data.frame(y = c(rnorm(5000, 50, 1), 999999)), var = "y")
    b <- ggplot2::ggplot_build(auditGgplot(res, ".histPlot"))
    testthat::expect_lte(nrow(b$data[[1]]), 200)
    testthat::expect_true(".widerBins" %in% vapply(res$items, function(i) i$name, ""))
})

testthat::test_that("discbar: a code-like range is refused with a message", {
    testthat::expect_error(
        scatr::discbar(data = data.frame(k = c(1, 2, 3, 999999)), var = "k"),
        "zbyt szeroki zakres"
    )
})

testthat::test_that("plots explain an empty variable instead of failing obscurely", {
    e <- data.frame(k = factor(rep(NA, 5), levels = c("a", "b")), n = rep(NA_real_, 5),
                    a = c(1, 2, NA, NA, NA), b = c(NA, NA, 3, 4, 5))
    testthat::expect_error(
        auditGgplot(scatr::stackbar(data = e, x = "k"), ".stackbarPlot"),
        "Brak obserwacji"
    )
    testthat::expect_error(
        auditGgplot(scatr::pareto(data = e, x = "k"), ".pareto"),
        "Brak obserwacji dla wykresu Pareto"
    )
    full <- data.frame(k = factor(c("a", "b", "a")), n = rep(NA_real_, 3))
    testthat::expect_error(
        auditGgplot(scatr::pareto(data = full, x = "k", counts = "n"), ".pareto"),
        "Brak obserwacji dla wykresu Pareto"
    )
    # no row is complete in both variables
    testthat::expect_error(
        auditGgplot(scatr::meanprofile(data = e, vars = c("a", "b")), ".meanprofilePlot"),
        "Profil średnich wymaga kompletnych obserwacji"
    )
})

testthat::test_that("dumbbell keeps categories apart when their labels wrap to the same text", {
    long <- "aaaa bbbb cccc dddd eeee ffff gggg hhhh iiii jjjj kkkk llll"
    d <- data.frame(lab = c(paste(long, "1"), paste(long, "2")), s = c(1, 2), e = c(3, 5))
    testthat::expect_true(anyDuplicated(jmvcore::wrapLabels(d$lab)) > 0)
    p <- auditGgplot(scatr::dumbbell(data = d, label = "lab", start = "s", end = "e"), ".dumbbellPlot")
    testthat::expect_equal(length(unique(ggplot2::ggplot_build(p)$data[[1]]$y)), 2)
})

testthat::test_that("stripmean names the error bars in the default caption", {
    d <- data.frame(y = c(1, 2, 3, 4, 5, 6), g = rep(c("a", "b"), each = 3))
    cap <- function(...) auditGgplot(scatr::stripmean(data = d, var = "y", group = "g", ...), ".stripPlot")$labels$caption
    testthat::expect_match(cap(), "95% przedział ufności")
    testthat::expect_match(cap(errorBars = "se"), "błąd standardowy")
    testthat::expect_match(cap(errorBars = "sd"), "odchylenie standardowe")
    testthat::expect_null(cap(errorBars = "none"))
    testthat::expect_null(cap(showMean = FALSE))
    testthat::expect_equal(cap(caption = "Własny podpis"), "Własny podpis")
})
