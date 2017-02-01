context("Test XY Plot")

test_that("Vignette volcano example runs", {
    load("volcano_glXYPlot.RData")
    expect_silent(glXYPlot(x=volcano_coef, y=volcano_lod, launch=FALSE))
    expect_silent(glXYPlot(x=volcano_coef, y=volcano_lod, counts=volcano_counts, launch=FALSE))
})

unlink("glimma-plots", recursive=TRUE)
