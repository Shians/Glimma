context("Test XY Plot")

test_that("Vignette volcano example runs", {
    load("volcano_glXYPlot.RData")
    expect_silent(glXYPlot(x=volcano_coef, y=volcano_lod, launch=FALSE))
    expect_silent(glXYPlot(x=volcano_coef, y=volcano_lod, counts=volcano_counts, launch=FALSE))
    expect_silent(glXYPlot(x=volcano_coef, y=volcano_lod, counts=volcano_counts, anno=volcano_anno, launch=FALSE))
})

test_that("XY Plot runs with vector or single column anno", {
	load("volcano_glXYPlot.RData")
	expect_silent(glXYPlot(x=volcano_coef, y=volcano_lod, anno=volcano_anno[, 1, drop=FALSE], launch=FALSE))
})

unlink("glimma-plots", recursive=TRUE)
