context("JSON Conversion")

test_that("factor conversion is correct", {
    x1 <- factor(c(1, 2, 3))
    x2 <- factor(c("a", "b"))
    x3 <- factor(c("a", "b"), levels=c("a", "b", "c"))
    expect_that(as.character(makeJson(x1)), equals("[\"1\",\"2\",\"3\"]"))
    expect_that(as.character(makeJson(x2)), equals("[\"a\",\"b\"]"))
    expect_that(as.character(makeJson(x3)), equals("[\"a\",\"b\",\"c\"]"))
})

test_that("list conversion is correct", {
    x1 <- list(a=c(1, 2), b=c("a", "b"))
    x2 <- list(a=factor(c(1, 2)), b=c(TRUE, FALSE))
    x3 <- list(a="string", b=1, c=TRUE, d=factor(1))
    x4 <- list(a=list(c="a", d=c(1, 2)), b=TRUE)
    expect_that(as.character(makeJson(x1)), equals("{\"a\":[1,2],\"b\":[\"a\",\"b\"]}"))
    expect_that(as.character(makeJson(x2)), equals("{\"a\":[\"1\",\"2\"],\"b\":[true,false]}"))
    expect_equal(as.character(makeJson(x3)), "{\"a\":\"string\",\"b\":1,\"c\":true,\"d\":\"1\"}")
    expect_equal(as.character(makeJson(x4)), "{\"a\":{\"c\":\"a\",\"d\":[1,2]},\"b\":true}")
})

test_that("data.frame conversion is correct", {
    df1 <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
    df2 <- data.frame(a = c("foo", "bar", "baz"), b = c(4, 5, 6))
    df3 <- data.frame(a = c("foo", "bar", "baz"), b = c("4", "5", "6"))
    df4 <- data.frame(a = c("foo", "bar", "baz"), b = c(TRUE, TRUE, FALSE))
    df5 <- data.frame(a = c("foo", "bar", NA), b = c("4", NA, "6"))

    expect_that(makeJson(df1), is_a("json"))
    expect_that(makeJson(df2), is_a("json"))
    expect_that(makeJson(df3), is_a("json"))

    expect_that(as.character(makeJson(df1)), equals("[{\"a\":1,\"b\":4},{\"a\":2,\"b\":5},{\"a\":3,\"b\":6}]"))

    expect_that(as.character(makeJson(df2)),
            equals("[{\"a\":\"foo\",\"b\":4},{\"a\":\"bar\",\"b\":5},{\"a\":\"baz\",\"b\":6}]"))

    expect_that(as.character(makeJson(df3)),
            equals("[{\"a\":\"foo\",\"b\":\"4\"},{\"a\":\"bar\",\"b\":\"5\"},{\"a\":\"baz\",\"b\":\"6\"}]"))

    expect_that(as.character(makeJson(df4, convert.logical=FALSE)),
            equals("[{\"a\":\"foo\",\"b\":true},{\"a\":\"bar\",\"b\":true},{\"a\":\"baz\",\"b\":false}]"))

    expect_that(as.character(makeJson(df4)),
            equals("[{\"a\":\"foo\",\"b\":\"TRUE\"},{\"a\":\"bar\",\"b\":\"TRUE\"},{\"a\":\"baz\",\"b\":\"FALSE\"}]"))

    expect_that(as.character(makeJson(df5)),
            equals("[{\"a\":\"foo\",\"b\":\"4\"},{\"a\":\"bar\",\"b\":\"NA\"},{\"a\":\"NA\",\"b\":\"6\"}]"))
})

test_that("Numeric conversion does not round to 0", {
  plotdata = data.frame(x=exp(log(10) * seq(log10(1e-100), log10(1), length.out=10000)), y=1:10000)
  plot = Glimma:::glScatter.default(plotdata, xval="x", yval="y")
  jsondf = jsonlite::fromJSON(plot$json)
  expect_false(any(jsondf$x == 0))
})
