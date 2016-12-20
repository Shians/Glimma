context("Text utility functions")

test_that("Text utilities functions correctly", {
    expect_equal(quotify("a"), "\"a\"")
    expect_equal(quotify(c("a", "b")), c("\"a\"", "\"b\""))

    expect_equal(firstChar("hello"), "h")
    expect_equal(lastChar("hello"), "o")

    expect_equal(char("hello", 1), "h")
    expect_equal(char("hello", 2), "e")
    expect_equal(char("hello", 0), "")
    expect_equal(char("hello", -1), "o")
    expect_equal(char("hello", -2), "l")

    expect_error(char("hello", 7))
    expect_error(char(1, 1))
})
