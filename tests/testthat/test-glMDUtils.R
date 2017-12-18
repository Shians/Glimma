context("glMDPlot Utilities")

test_that("convert status to colours", {
    expect_equal(
        convertStatusToCols(c(-1, 0, 1), c("#00bfff", "#858585", "#ff3030")),
        c("#00bfff", "#858585", "#ff3030")
    )

    expect_equal(
        convertStatusToCols(c(0, 0, 0), c("#00bfff", "#858585", "#ff3030")),
        c("#858585", "#858585", "#858585")
    )

    expect_error(
        convertStatusToCols(c(1, 2), c("#00bfff", "#858585", "#ff3030"))
    )

    expect_error(
        convertStatusToCols(c(-1, 0, 1), c("#00bfff", "#858585"))
    )
})

test_that("group initialisation", {
    expect_equal(initialise_groups(NULL, 3), 1:3)
})

test_that("sort insignificant points to the top", {
    plotting_data <- data.frame(
        sample = c(1, 2, 3),
        cols = c("#00bfff", "#858585", "#ff3030")
    )

    expected_output <- plotting_data[c(2, 1, 3), ]

    expect_equal(
        sortInsigPointsToTop(plotting_data, "#858585"),
        expected_output
    )
})

test_that("glMDRmd runs", {
    expect_silent(glMDRmd())
})

test_that("checkSideMainPresent flags errors", {
    anno <- data.frame(
        Symbol = c("gene1", "gene2"),
        Chr = c("chr1", "chr2")
    )
    x <- list(table = data.frame(avgExpr = c(1, 2), logFC = c(1, 2)))

    class(x) <- "DGELRT"
    expect_error(checkSideMainPresent("GeneID", anno, x))

    x$table <- NULL
    expect_silent(checkSideMainPresent("Symbol", anno, x))

    class(x) <- "LMArrayLM"
    x$genes <- data.frame(avgExpr = c(1, 2), logFC = c(1, 2))
    expect_error(checkSideMainPresent("GeneID", anno, x))

    x$table <- NULL
    expect_silent(checkSideMainPresent("Symbol", anno, x))

    class(x) <- "data.frame"
    expect_error(checkSideMainPresent("GeneID", anno, x))
})