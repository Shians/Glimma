context("Test MD Plot")

test_that("MD Plot runs for voom", {
    load("test_data_voom.rda")

    counts <- counts$counts
    display.columns <- c("Symbols", "GeneID")

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
    load("test_data_DGELRT.rda")

    counts <- counts$counts
    display.columns <- c("Symbols", "GeneID")

    expect_silent(glMDPlot(qlf, anno=geneanno, main="MDPlot", launch=FALSE))

    expect_silent(glMDPlot(qlf, counts=counts, anno=geneanno,
            samples=1:6, status=is.de, main="MDPlot", launch=FALSE))

    expect_silent(glMDPlot(qlf, counts=counts, anno=geneanno, groups=genotypes,
            samples=1:6, status=is.de, launch=FALSE))

    expect_silent(glMDPlot(qlf, counts=counts, anno=geneanno, groups=genotypes,
            samples=1:6, display.columns=display.columns, status=is.de, main="MDPlot", launch=FALSE))
})

test_that("MD Plot runs for DGEExact", {
    load("test_data_DGEExact.rda")

    counts <- counts$counts
    display.columns <- c("Symbols", "GeneID")

    expect_silent(glMDPlot(et, main="MDPlot", launch=FALSE))

    expect_silent(glMDPlot(et, counts=counts,
        samples=1:6, status=is.de, main="MDPlot", launch=FALSE))

    expect_silent(glMDPlot(et, counts=counts, anno=geneanno, groups=genotypes,
        samples=1:6, display.columns=display.columns, status=is.de, main="MDPlot", launch=FALSE))
})

unlink("glimma-plots", recursive=TRUE)
