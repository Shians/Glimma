context("Argument checking functions")

test_that("Checking functions are correct", {
    df <- data.frame(x=c(1, 2), y=c(1, 2))
    expect_that(checkThat(df, hasCols(c("x", "y"))), is_true())
    expect_that(checkThat(df, hasCols("a")), throws_error())
    expect_that(checkThat(df, hasCols("a"), type="warn"), gives_warning())

    mat <- matrix(1, nrow=2, ncol=2)
    rownames(mat) <- c("x", "y")
    expect_that(checkThat(mat, hasRows(c("x", "y"))), is_true())

    x <- list()
    df <- data.frame()
    expect_that(checkThat(x, isClass("list")), is_true())
    expect_that(checkThat(df, isClass("data.frame")), is_true())
    expect_that(checkThat(x, isClass("data.frame")), throws_error())

    expect_that(checkThat("a", isString), is_true())
    expect_that(checkThat("a", isNumeric), throws_error("Argument must be of type numeric"))

    expect_that(checkThat(1, isNumeric), is_true())
    expect_that(checkThat(1, isString), throws_error("Argument must be of type character"))

    x <- c(1, 2)
    y <- c(1, 1)
    expect_that(checkThat(x, isUnique), is_true())
    expect_that(checkThat(y, isUnique), throws_error())

    expect_that(checkThat(TRUE, isLogical), is_true())
    expect_that(checkThat(TRUE, isFactor), throws_error("Argument must be of type factor"))

    expect_that(checkThat(factor(c(1, 2)), isFactor), is_true())

    x <- c(1, 2)
    expect_that(checkThat(x, hasLength(2)), is_true())
    expect_that(checkThat(x, hasLength(1)), throws_error())

    expect_that(checkThat("a", sameAs("a")), is_true())
    expect_that(checkThat("a", sameAs("b")), throws_error())

    expect_that(checkThat("a", isIn(c("a", "b", "c"))), is_true())
    expect_that(checkThat("d", isIn(c("a", "b", "c"))), throws_error())

    expect_error(checkThat(NULL, notNull))
    expect_equal(checkThat(1, notNull), TRUE)
    expect_equal(checkThat(c(1, 2), notNull), TRUE)
    expect_equal(checkThat("NULL", notNull), TRUE)
    expect_equal(checkThat(c("NULL", "b"), notNull), TRUE)
})
