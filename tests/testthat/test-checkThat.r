context("Argument checking functions")

test_that("Checking functions are correct", {
    df <- data.frame(x=c(1, 2), y=c(1, 2))
    expect_true(checkThat(df, hasCols(c("x", "y"))))
    expect_error(checkThat(df, hasCols("a")))
    expect_warning(checkThat(df, hasCols("a"), type="warn"))

    mat <- matrix(1, nrow=2, ncol=2)
    rownames(mat) <- c("x", "y")
    expect_true(checkThat(mat, hasRows(c("x", "y"))))

    x <- list()
    df <- data.frame()
    expect_true(checkThat(x, isClass("list")))
    expect_true(checkThat(df, isClass("data.frame")))
    expect_error(checkThat(x, isClass("data.frame")))

    expect_true(checkThat("a", isString))
    expect_that(checkThat("a", isNumeric), throws_error("Argument must be of type numeric"))

    expect_true(checkThat(1, isNumeric))
    expect_that(checkThat(1, isString), throws_error("Argument must be of type character"))

    x <- c(1, 2)
    y <- c(1, 1)
    expect_true(checkThat(x, isUnique))
    expect_error(checkThat(y, isUnique))

    expect_true(checkThat(TRUE, isLogical))
    expect_that(checkThat(TRUE, isFactor), throws_error("Argument must be of type factor"))

    expect_true(checkThat(factor(c(1, 2)), isFactor))

    x <- c(1, 2)
    expect_true(checkThat(x, hasLength(2)))
    expect_error(checkThat(x, hasLength(1)))

    expect_true(checkThat("a", sameAs("a")))
    expect_error(checkThat("a", sameAs("b")))

    expect_true(checkThat("a", isIn(c("a", "b", "c"))))
    expect_error(checkThat("d", isIn(c("a", "b", "c"))))

    expect_error(checkThat(NULL, notNull))
    expect_true(checkThat(1, notNull))
    expect_true(checkThat(c(1, 2), notNull))
    expect_true(checkThat("NULL", notNull))
    expect_true(checkThat(c("NULL", "b"), notNull))
})
