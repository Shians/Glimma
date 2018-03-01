context("Test XY Plot")

test_that("Vignette volcano example runs", {
    load("volcano_glXYPlot.RData")

    temp_dir <- normalizePath(tempdir())

    expect_silent(
        glXYPlot(x=volcano_coef, y=volcano_lod, launch=FALSE, path=temp_dir)
    )
    expect_silent(
        glXYPlot(x=volcano_coef, y=volcano_lod, counts=volcano_counts, launch=FALSE, path=temp_dir)
    )
    expect_silent(
        glXYPlot(x=volcano_coef, y=volcano_lod, counts=volcano_counts, anno=volcano_anno, launch=FALSE, path=temp_dir)
    )
})

test_that("XY Plot runs with vector or single column anno", {
	load("volcano_glXYPlot.RData")

    temp_dir <- normalizePath(tempdir())

	expect_silent(glXYPlot(x=volcano_coef, y=volcano_lod, anno=volcano_anno[, 1, drop=FALSE], launch=FALSE, path=temp_dir))
})

test_that("XY Plot runs with spaces in labels", {
    load("volcano_glXYPlot.RData")

    temp_dir <- normalizePath(tempdir())

    expect_silent(
        glXYPlot(x=volcano_coef, y=volcano_lod, xlab = "coef", ylab = "lod", launch=FALSE, path=temp_dir)
    )

    expect_silent(
        glXYPlot(x=volcano_coef, y=volcano_lod, xlab = "volcano coef", ylab = "volcano lod", launch=FALSE, path=temp_dir)
    )
})
