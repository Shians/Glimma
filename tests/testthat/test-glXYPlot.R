context("Test XY Plot")

test_that("Vignette volcano example runs", {
    load("volcano_glXYPlot.RData")
    glXYPlot(x=volcano_coef, y=volcano_lod, launch=FALSE)
})

unlink("glimma-plots", recursive=TRUE)
