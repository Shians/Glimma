context("Test MD Plot")

test_that("Helper functions run as expected", {
    status <- c(0, 1, -1)
    cols <- c("blue", "black", "red")

    expect_equal(convertStatusToCols(status, cols), c("black", "red", "blue"))
    expect_equal(initialiseGroups(5), 1:5)
    expect_equal(initialiseGroups(NULL), NULL)
})

test_that("MD Plot runs for voom", {
    load("test_data_voom.RData")
    counts <- counts$counts
    display.columns <- c("Symbols", "GeneID")

    expect_silent(glMDPlot(fit, counts=counts, launch=FALSE))
    expect_silent(glMDPlot(fit, counts=counts, status=is.de, launch=FALSE))
    expect_silent(glMDPlot(fit, counts=counts, anno=geneanno, launch=FALSE))

    expect_silent(
        glMDPlot(fit, counts=counts, anno=geneanno, groups=genotypes,
            display.columns=display.columns, launch=FALSE))

    expect_silent(
        glMDPlot(fit, counts=counts, anno=geneanno, groups=genotypes,
            samples=1:6, status=is.de, display.columns=display.columns, launch=FALSE))

    expect_error(glMDPlot(fit, counts=counts, samples=1:2, anno=geneanno, launch=FALSE))
})

test_that("MD Plot runs for DGELRT", {
    load("test_data_DGELRT.RData")
    counts <- counts$counts
    display.columns <- c("Symbols", "GeneID")

    expect_silent(glMDPlot(qlf, anno=geneanno, main="MDPlot", launch=FALSE))

    expect_silent(glMDPlot(qlf, anno=geneanno, main="MDPlot", launch=FALSE))

    expect_silent(glMDPlot(qlf, counts=counts, main="MDPlot", launch=FALSE))

    expect_silent(glMDPlot(qlf, counts=counts, anno=geneanno,
            samples=1:6, status=is.de, main="MDPlot", launch=FALSE))

    expect_silent(glMDPlot(qlf, counts=counts, anno=geneanno, groups=genotypes,
            samples=1:6, status=is.de, launch=FALSE))

    expect_silent(glMDPlot(qlf, counts=counts, anno=geneanno, groups=genotypes,
            samples=1:6, display.columns=display.columns, status=is.de, main="MDPlot", launch=FALSE))

    load("little_mait_DGELRT.RData")

    expect_silent(glMDPlot(little_mait, launch=FALSE))
    expect_silent(glMDPlot(little_mait, anno=little_mait$genes, launch=FALSE))
})

test_that("MD Plot runs for DGEExact", {
    load("test_data_DGEExact.RData")
    counts <- counts$counts
    display.columns <- c("Symbols", "GeneID")

    expect_silent(glMDPlot(et, main="MDPlot", launch=FALSE))

    expect_silent(glMDPlot(et, counts=counts,
        samples=1:6, status=is.de, main="MDPlot", launch=FALSE))

    expect_silent(glMDPlot(et, counts=counts, anno=geneanno, groups=genotypes,
        samples=1:6, display.columns=display.columns, status=is.de, main="MDPlot", launch=FALSE))
})

unlink("glimma-plots", recursive=TRUE)
