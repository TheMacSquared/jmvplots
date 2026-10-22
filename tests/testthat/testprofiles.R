#' Profiles category (jUPWR): means profile, dumbbell and slope chart

profilesGgplot <- function(res, renderFun) {
    analysis <- res$.__enclos_env__$private$.parent
    priv <- analysis$.__enclos_env__$private
    opts <- analysis$options
    gt <- utils::getFromNamespace("getGlobalTheme", "jmvcore")(opts$theme, opts$palette)
    priv[[renderFun]](res$plot, gt$ggtheme, gt$theme)
}

testthat::test_that("meanprofile: means per group and variable, z on the whole sample", {
    res <- scatr::meanprofile(
        data = iris, vars = c("Sepal.Length", "Petal.Length"), group = "Species", scale = "z"
    )
    state <- res$plot$state
    z <- scale(iris$Sepal.Length)[, 1]

    testthat::expect_equal(nrow(state), 6)
    testthat::expect_equal(
        state$mean[state$group == "setosa" & state$varIndex == 1],
        mean(z[iris$Species == "setosa"])
    )
    se <- stats::sd(z[iris$Species == "setosa"]) / sqrt(50)
    testthat::expect_equal(
        state$half[state$group == "setosa" & state$varIndex == 1],
        stats::qt(0.975, 49) * se
    )
    testthat::expect_s3_class(profilesGgplot(res, ".meanprofilePlot"), "ggplot")
})

testthat::test_that("meanprofile: original scale without a group, complete cases", {
    df <- data.frame(a = c(1, 2, 3, NA), b = c(4, 5, 6, 7))
    res <- scatr::meanprofile(data = df, vars = c("a", "b"), errorBars = "none")
    testthat::expect_equal(res$plot$state$mean, c(2, 5))
})

testthat::test_that("dumbbell: categories averaged and ordered by change", {
    df <- data.frame(
        k = c("A", "A", "B", "C"), przed = c(10, 12, 5, 8), po = c(20, 22, 4, 9)
    )
    res <- scatr::dumbbell(data = df, label = "k", start = "przed", end = "po")
    state <- res$plot$state
    testthat::expect_equal(state$start[state$label == "A"], 11)
    testthat::expect_equal(state$end[state$label == "A"], 21)

    p <- profilesGgplot(res, ".dumbbellPlot")
    # bottom to top: the smallest change first
    testthat::expect_equal(levels(p$data$label), c("B", "C", "A"))
})

testthat::test_that("slope: one line per row without labels, direction of change", {
    df <- data.frame(przed = c(1, 5, 3), po = c(2, 4, 3))
    res <- scatr::slope(data = df, start = "przed", end = "po")
    p <- profilesGgplot(res, ".slopePlot")
    lines <- ggplot2::layer_data(p, 1)
    testthat::expect_equal(length(unique(lines$group)), 3)
    testthat::expect_equal(length(unique(lines$colour)), 3)
})

testthat::test_that("slope: labels are spread apart", {
    df <- data.frame(k = c("A", "B", "C"), przed = c(1, 1, 1), po = c(10, 10.1, 5))
    res <- scatr::slope(data = df, label = "k", start = "przed", end = "po")
    p <- profilesGgplot(res, ".slopePlot")
    labels <- ggplot2::layer_data(p, 3)
    gaps <- diff(sort(labels$y))
    testthat::expect_true(all(gaps >= 0.075 * 9 - 1e-9))
})
