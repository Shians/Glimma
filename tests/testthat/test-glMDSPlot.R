context("Test MDS Plot")

test_that("MDS Plot runs", {
    load("test_data_MDS.rda")

    groups <- data.frame(Grouping1=c(rep(1, 40), rep(2, 30)),
                       Grouping2=c(rep(1, 20), rep(2, 30), rep(3, 20)),
                       Grouping3=sample(1:4, 70, replace=TRUE))

    expect_silent(glMDSPlot(all_counts, launch=FALSE))
    expect_silent(glMDSPlot(all_counts, groups=groups[, 1], launch=FALSE))
    expect_silent(glMDSPlot(all_counts, groups=groups, launch=FALSE))
})

unlink("glimma-plots", recursive=TRUE)
