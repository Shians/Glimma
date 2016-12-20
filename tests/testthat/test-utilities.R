context("Utilities")

test_that("char extraction", {
    expect_equal(char("abc", 1), "a")
    expect_equal(char("abc", 2), "b")
})

test_that("unique conversion works", {
    expect_equal(makeUnique(c(1, 1, 2)), c("1.1", "1.2", "2"))
    expect_equal(makeUnique(c("a", "a", "b")), c("a.1", "a.2", "b"))
})

test_that("Row and column extractions work", {
    x <- matrix(1:9, nrow=3, ncol=3)

    expect_equal(getCols(x, 1), matrix(c(1, 2, 3), ncol=1))
    expect_equal(getCols(x, 1:2), matrix(1:6, ncol=2))
    expect_equal(getRows(x, 1), matrix(c(1, 4, 7), nrow=1))
})
