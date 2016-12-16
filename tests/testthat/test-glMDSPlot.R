context("Test MDS Plot")

test_that("MDS Plot runs for matrix", {
    load("test_data_MDS_DGEList.rda")
    counts <- all_counts$counts

    groups <- data.frame(Grouping1=c(rep(1, 40), rep(2, 30)),
                       Grouping2=c(rep(1, 20), rep(2, 30), rep(3, 20)),
                       Grouping3=sample(1:4, 70, replace=TRUE))

    expect_silent(glMDSPlot(counts, launch=FALSE))
    expect_silent(glMDSPlot(counts, groups=groups[, 1], launch=FALSE))
    expect_silent(glMDSPlot(counts, groups=groups, launch=FALSE))
})

test_that("MDS Plot runs for DGEList", {
    load("test_data_MDS_DGEList.rda")

    groups <- data.frame(Grouping1=c(rep(1, 40), rep(2, 30)),
                       Grouping2=c(rep(1, 20), rep(2, 30), rep(3, 20)),
                       Grouping3=sample(1:4, 70, replace=TRUE))

    expect_silent(glMDSPlot(all_counts, launch=FALSE))
    expect_silent(glMDSPlot(all_counts, groups=groups[, 1], launch=FALSE))
    expect_silent(glMDSPlot(all_counts, groups=groups, launch=FALSE))
})

test_that("MDS Plot runs for DESeqDataSet", {
    library(DESeq2)
    library(S4Vectors)
    load("test_data_MDS_DESeq.rda")

    expect_silent(glMDSPlot(dds, launch=FALSE))
})

unlink("glimma-plots", recursive=TRUE)
