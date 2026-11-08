testthat::test_that("summary plots render as vector resources and export to SVG/PDF/PNG", {
    d <- data.frame(grupa = factor(rep(c("A", "B", "C"), 40)),
                    warstwa = factor(rep(c("D", "E"), each = 3, length.out = 120)),
                    x = 10 + (0:119 %% 20) + sin(0:119),
                    y = 5 + (0:119 %% 20) * 0.6 + cos(0:119),
                    z = 3 + (0:119 %% 11) * 0.8 + sin((0:119) * 0.5),
                    licznosc = 0:119 %% 8 + 1)
    cases <- list(
        jmvbar = list(mode = "categorical", catvar = "grupa"),
        jmvhist = list(var = "x", group = "grupa"),
        discbar = list(var = "licznosc"),
        dens = list(var = "x", group = "grupa"),
        ridge = list(var = "x", group = "grupa"),
        mosaic = list(x = "grupa", y = "warstwa"),
        stackbar = list(x = "grupa", segments = "warstwa"),
        circbar = list(x = "grupa", y = "x"),
        lollipop = list(x = "grupa", y = "x"),
        pareto = list(x = "grupa"),
        heatmap = list(x = "grupa", y = "warstwa", value = "x"),
        corrgram = list(vars = c("x", "y", "z")),
        hexbin = list(x = "x", y = "y"),
        radar = list(vars = c("x", "y", "z"), group = "grupa"),
        treemap = list(group = "grupa", group2 = "warstwa"),
        waffle = list(group = "grupa", group2 = "warstwa"),
        meanprofile = list(vars = c("x", "y", "z"), group = "grupa"))
    for (name in names(cases)) {
        result <- do.call(getExportedValue("scatr", name), c(list(data = d), cases[[name]]))
        testthat::expect_identical(result$plot$mode, "vector", info = name)
        for (extension in c("svg", "pdf", "png")) {
            target <- tempfile(fileext = paste0(".", extension))
            suppressWarnings(suppressMessages(result$plot$saveAs(target)))
            testthat::expect_gt(file.info(target)$size, 100)
            if (extension == "svg") {
                content <- paste(readLines(target, warn = FALSE), collapse = "\n")
                testthat::expect_match(content, "<svg", info = name)
                testthat::expect_match(content, "<path", info = name)
            }
            unlink(target)
        }
    }
})
