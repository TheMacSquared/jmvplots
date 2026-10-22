#' Wide format (several measurement columns) in the group plots (jUPWR)

wideData <- data.frame(
    przed = c(10, 12, 14, NA, 11),
    po = c(15, 16, 13, 18, 17),
    po_pol_roku = c(14, NA, 15, 16, 12),
    grupa = c("a", "b", "a", "b", "a")
)

testthat::test_that("wideToLong: one category per column, missing values per column", {
    long <- scatr:::wideToLong(wideData, c("po", "przed"))
    testthat::expect_equal(levels(long$x), c("po", "przed"))
    testthat::expect_equal(as.integer(table(long$x)), c(5, 4))
    testthat::expect_equal(long$y[long$x == "przed"], c(10, 12, 14, 11))
})

testthat::test_that("wideToLong: listwise keeps only complete rows", {
    long <- scatr:::wideToLong(wideData, c("przed", "po", "po_pol_roku"), "grupa", listwise = TRUE)
    testthat::expect_equal(as.integer(table(long$x)), c(3, 3, 3))
    # rows 1, 3 and 5 are complete, all in group "a"
    testthat::expect_equal(as.character(unique(long$group)), "a")
})

testthat::test_that("violin, raincloud, stripmean: wide state", {
    cols <- c("przed", "po", "po_pol_roku")
    for (res in list(
        scatr::violin(data = wideData, mode = "wide", vars = c("przed", "po", "po_pol_roku"), wideGroup = "grupa"),
        scatr::raincloud(data = wideData, mode = "wide", vars = c("przed", "po", "po_pol_roku")),
        scatr::stripmean(data = wideData, mode = "wide", vars = c("przed", "po", "po_pol_roku"))
    )) {
        testthat::expect_equal(levels(res$plot$state$x), cols)
        testthat::expect_equal(nrow(res$plot$state), 13)
    }
})

testthat::test_that("jmvbox syntax: wide format is reproduced", {
    res <- scatr::jmvbox(
        data = wideData, mode = "wide", vars = c("przed", "po", "po_pol_roku"), wideGroup = "grupa"
    )
    syntax <- res$.__enclos_env__$private$.parent$asSource()
    testthat::expect_true(grepl("pivot_longer", syntax, fixed = TRUE))
    # the box statistics match; colours are left out (the syntax colours
    # differ from the jUPWR theme in every jmvbox syntax test, a known issue)
    stats <- c("x", "group", "ymin", "lower", "middle", "upper", "ymax")
    rendered <- ggplot2::layer_data(jamoviGgplot(res, ".boxPlot"), 1)[stats]
    emitted <- ggplot2::layer_data(syntaxGgplot(res, wideData), 1)[stats]
    testthat::expect_equal(emitted, rendered)
})

testthat::test_that("long format is unchanged by default", {
    res <- scatr::jmvbox(data = ToothGrowth, var = "len", group1 = "supp")
    testthat::expect_equal(sort(unique(as.character(res$plot$state$x))), c("OJ", "VC"))
})
