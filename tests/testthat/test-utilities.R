context("Utilities")

test_that("char extraction", {
    expect_equal(char("abc", 1), "a")
    expect_equal(char("abc", 2), "b")
})

test_that("hex colour tools are correct", {
    expect_false(is.hex("#12345"))
    expect_false(is.hex("#1234567"))
    expect_false(is.hex("#123456789"))
    expect_false(is.hex("#1122gg"))

    expect_true(is.hex("#123456"))
    expect_true(is.hex("#ffffff"))
    expect_true(is.hex("#FFFFFF"))

    expect_equal(as.hexcol("red"), "#ff0000")
    expect_equal(as.hexcol("green"), "#00ff00")
    expect_equal(as.hexcol("blue"), "#0000ff")

    expect_equal(as.hexcol(1), "#000000")
    expect_equal(as.hexcol(2), "#ff0000")
})

test_that("unique conversion works", {
    expect_equal(makeUnique(c(1, 1, 2)), c("1.1", "1.2", "2"))
    expect_equal(makeUnique(c("a", "a", "b")), c("a.1", "a.2", "b"))
})
