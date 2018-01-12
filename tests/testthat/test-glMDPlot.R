context("Test MD Plot")

test_that("id.column deprecation warning works", {
    temp_dir <- normalizePath(tempdir())
    load("test_data_voom.RData")

    counts <- counts$counts
    display.columns <- c("Symbols", "GeneID")

    expect_error(glMDPlot(fit, counts=counts, id.column="ENTREZID", launch=FALSE, path=temp_dir))
})

test_that("Helper functions run as expected", {
    temp_dir <- normalizePath(tempdir())

    status <- c(0, 1, -1)
    cols <- c("blue", "black", "red")

    expect_equal(convertStatusToCols(status, cols), c("black", "red", "blue"))
    expect_equal(initialise_groups(NULL, 5), 1:5)
    expect_equal(initialise_groups(NULL, NULL), NULL)
})

test_that("MD Plot runs for DGELRT", {
    temp_dir <- normalizePath(tempdir())

    load("test_data_DGELRT.RData")
    counts <- counts$counts
    display.columns <- c("Symbols", "GeneID")

    expect_warning(glMDPlot(qlf, counts=counts, main="MDPlot", launch=FALSE, path = temp_dir))

    # common arguments for working tests
    glMDPlot_core <- pryr::partial(
        glMDPlot,
        x = qlf,
        anno = geneanno,
        main = "MDPlot",
        launch = FALSE,
        path = temp_dir
    )

    expect_silent(glMDPlot_core())
    expect_silent(glMDPlot_core(xlab="foo", ylab="bar"))
    expect_silent(glMDPlot_core(side.xlab="foo", side.ylab="bar"))

    # add counts
    glMDPlot_with_counts <- pryr::partial(
        glMDPlot_core,
        count = counts
    )

    expect_silent(glMDPlot_with_counts(samples=1:6, status=is.de))
    expect_silent(glMDPlot_with_counts(samples=1:6, status=is.de, sample.cols=rep(c(1,2), c(3, 3))))
    expect_silent(glMDPlot_with_counts(groups=genotypes, samples=1:6, status=is.de))
    expect_silent(glMDPlot_with_counts(groups=genotypes, samples=1:6, display.columns=display.columns, status=is.de))

    load("little_mait_DGELRT.RData")

    expect_silent(glMDPlot(little_mait, launch=FALSE, path=temp_dir))
    expect_silent(glMDPlot(little_mait, anno=little_mait$genes, launch=FALSE, path=temp_dir))
})

test_that("MD Plot runs for DGEExact", {
    temp_dir <- normalizePath(tempdir())

    load("test_data_DGEExact.RData")
    counts <- counts$counts
    display.columns <- c("Symbols", "GeneID")

    # common arguments for working tests
    glMDPlot_core <- pryr::partial(
        glMDPlot,
        x = et,
        main = "MDPlot",
        launch = FALSE,
        path = temp_dir
    )

    expect_silent(glMDPlot_core())
    expect_silent(glMDPlot_core(xlab="foo", ylab="bar"))
    expect_silent(glMDPlot_core(side.xlab="foo", side.ylab="bar"))

    # add counts
    glMDPlot_with_counts_samples_status <- pryr::partial(
        glMDPlot_core,
        count = counts,
        samples = 1:6,
        status=is.de
    )

    expect_silent(glMDPlot_with_counts_samples_status())
    expect_silent(glMDPlot_with_counts_samples_status(anno=geneanno, groups=genotypes, display.columns=display.columns))
})

test_that("MD Plot runs for MArrayLM", {
    temp_dir <- normalizePath(tempdir())

    load("test_data_voom.RData")
    counts <- counts$counts
    display.columns <- c("Symbols", "GeneID")

    # common arguments for warning tests
    glMDPlot_warning <- pryr::partial(
        glMDPlot,
        x = fit,
        counts = counts,
        launch = FALSE,
        path = temp_dir
    )
    # No anno warning
    expect_warning(glMDPlot_warning())
    expect_warning(glMDPlot_warning(status=is.de))

    # common arguments for working tests
    glMDPlot_core <- pryr::partial(
        glMDPlot,
        x = fit,
        anno = geneanno,
        counts = counts,
        launch = FALSE,
        path = temp_dir
    )

    expect_silent(glMDPlot_core())
    expect_silent(glMDPlot_core(xlab="foo", ylab="bar"))
    expect_silent(glMDPlot_core(side.xlab="foo", side.ylab="bar"))
    expect_silent(glMDPlot_core(groups=genotypes, display.columns=display.columns))
    expect_silent(glMDPlot_core(groups=genotypes, samples=1:6, status=is.de, display.columns=display.columns))

    expect_error(glMDPlot(fit, counts=counts, samples=1:2, anno=geneanno, launch=FALSE, path = temp_dir))

    load("invalid_names_glMDPlot.RData")

    expect_silent(glMDPlot(invalid_names_fit, anno=invalid_names_anno, side.main="ENTREZID", launch=FALSE, path = temp_dir))
})

test_that("MD Plot runs for DESeqDataSet", {
    temp_dir <- normalizePath(tempdir())

    library(DESeq2)
    load("test_data_DESeqDataSet.RData")

    # common arguments for working tests
    glMDPlot_core <- pryr::partial(
        glMDPlot,
        x = lymphoma_dds,
        anno = lymphoma_anno,
        groups = lymphoma_genotypes,
        launch = FALSE,
        path = temp_dir
    )

    expect_silent(glMDPlot_core())
    expect_silent(glMDPlot_core(samples=1:7, status=lymphoma_status))

    # common arguments for working tests
    glMDPlot_deseqresults <- pryr::partial(
        glMDPlot,
        x = DESeq2::results(lymphoma_dds),
        counts = DESeq2::counts(lymphoma_dds),
        anno = lymphoma_anno,
        groups = lymphoma_genotypes,
        launch = FALSE,
        path = temp_dir
    )

    expect_silent(glMDPlot_deseqresults())
    expect_silent(glMDPlot_deseqresults(samples=1:7, status=lymphoma_status))

})
