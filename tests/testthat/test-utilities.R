context("Utilities")

test_that("char extraction", {
    expect_equal(char("abc", 1), "a")
    expect_equal(char("abc", 2), "b")
})

test_that("unique conversion works", {
    expect_equal(makeUnique(c(1, 1, 2)), c("1.1", "1.2", "2"))
    expect_equal(makeUnique(c("a", "a", "b")), c("a.1", "a.2", "b"))
})
