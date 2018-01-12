context("Utilities")

test_that("char extraction", {
    expect_equal(char("abc", 1), "a")
    expect_equal(char("abc", 2), "b")
})

test_that("unique conversion works", {
    expect_equal(makeUnique(c(1, 1, 2)), c("1.1", "1.2", "2"))
    expect_equal(makeUnique(c("a", "a", "b")), c("a.1", "a.2", "b"))
    expect_equal(makeUnique(c(NA, NA)), c("NA.1", "NA.2"))
})

test_that("Row and column extractions work", {
    x <- matrix(1:9, nrow=3, ncol=3)

    expect_equal(getCols(x, 1), matrix(c(1, 2, 3), ncol=1))
    expect_equal(getCols(x, 1:2), matrix(1:6, ncol=2))
    expect_equal(getRows(x, 1), matrix(c(1, 4, 7), nrow=1))
})

test_that("path maker works", {
    expect_error(pathMaker("path"))

    p_maker <- pathMaker("path/")

    expect_equal(p_maker("to"), "path/to")
    expect_equal(p_maker("to/elsewhere"), "path/to/elsewhere")
})

test_that("seq_along variants work", {
    data(mtcars)

    expect_identical(seq_rows(mtcars), 1:32)

    expect_identical(seq_cols(mtcars), 1:11)
})