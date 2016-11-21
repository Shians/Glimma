context("JSON Conversion")

test_that("factor conversion is correct", {
    x1 <- factor(c(1, 2, 3))
    x2 <- factor(c("a", "b"))
    x3 <- factor(c("a", "b"), levels=c("a", "b", "c"))
    expect_that(makeJson(x1), equals("[\"1\",\"2\",\"3\"]"))
    expect_that(makeJson(x2), equals("[\"a\",\"b\"]"))
    expect_that(makeJson(x3), equals("[\"a\",\"b\",\"c\"]"))
})

test_that("list conversion is correct", {
    x1 <- list(a=c(1, 2), b=c("a", "b"))
    x2 <- list(a=factor(c(1, 2)), b=c(TRUE, FALSE))
    expect_that(as.character(makeJson(x1)), equals("{\"a\":[1,2],\"b\":[\"a\",\"b\"]}"))
    expect_that(as.character(makeJson(x2)), equals("{\"a\":[\"1\",\"2\"],\"b\":[true,false]}"))
})

test_that("data.frame conversion is correct", {
    df1 <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
    df2 <- data.frame(a = c("foo", "bar", "baz"), b = c(4, 5, 6))
    df3 <- data.frame(a = c("foo", "bar", "baz"), b = c("4", "5", "6"))
    df4 <- data.frame(a = c("foo", "bar", "baz"), b = c(TRUE, TRUE, FALSE))

    expect_that(makeJson(df1), is_a("json"))
    expect_that(makeJson(df2), is_a("json"))
    expect_that(makeJson(df3), is_a("json"))

    expect_that(as.character(makeJson(df1)), equals("[{\"a\":1,\"b\":4},{\"a\":2,\"b\":5},{\"a\":3,\"b\":6}]"))

    expect_that(as.character(makeJson(df2)),
            equals("[{\"a\":\"foo\",\"b\":4},{\"a\":\"bar\",\"b\":5},{\"a\":\"baz\",\"b\":6}]"))

    expect_that(as.character(makeJson(df3)),
            equals("[{\"a\":\"foo\",\"b\":\"4\"},{\"a\":\"bar\",\"b\":\"5\"},{\"a\":\"baz\",\"b\":\"6\"}]"))

    expect_that(as.character(makeJson(df4)),
            equals("[{\"a\":\"foo\",\"b\":\"TRUE\"},{\"a\":\"bar\",\"b\":\"TRUE\"},{\"a\":\"baz\",\"b\":\"FALSE\"}]"))
})
