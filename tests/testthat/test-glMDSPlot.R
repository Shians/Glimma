context("Test MDS Plot")

test_that("MDS Plot runs for matrix", {
    load("pasilla.RData")

    expect_silent(glMDSPlot(countData, launch=FALSE))
    expect_silent(glMDSPlot(countData, groups=colData[, 1], launch=FALSE))
    expect_silent(glMDSPlot(countData, groups=colData, launch=FALSE))

    countData[1, ] <- NaN
    expect_warning(glMDSPlot(countData, groups=colData, launch=FALSE))
})

test_that("MDS Plot runs for DGEList", {
    load("test_data_MDS_DGEList.RData")

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
    load("pasilla.RData")
    rownames(colData) <- sub("fb", "", rownames(colData))
    dds <- DESeqDataSetFromMatrix(countData = countData,
                    colData = rbind(colData[4:7, ], colData[1:3, ]),
                    design = ~condition)

    expect_silent(glMDSPlot(dds, launch=FALSE))
})

unlink("glimma-plots", recursive=TRUE)
