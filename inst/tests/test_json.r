context("JSON Conversion")

test_that("data.frame conversion is correct", {
    df1 <- data.frame(a = c(1,2,3), b = c(4, 5, 6))
    df2 <- data.frame(a = c("foo","bar","baz"), b = c(4, 5, 6))
    df3 <- data.frame(a = c("foo","bar","baz"), b = c("4", "5", "6"))

    expect_that(makeDFJson(df1), is_a("json"))
    expect_that(makeDFJson(df2), is_a("json"))

    expect_that(as.character(makeDFJson(df1)), equals("[{\"a\":1,\"b\":4},{\"a\":2,\"b\":5},{\"a\":3,\"b\":6}]"))
    expect_that(as.character(makeDFJson(df2)), equals("[{\"a\":\"foo\",\"b\":4},{\"a\":\"bar\",\"b\":5},{\"a\":\"baz\",\"b\":6}]"))
    expect_that(as.character(makeDFJson(df2)), equals("[{\"a\":\"foo\",\"b\":\"4\"},{\"a\":\"bar\",\"b\":\"5\"},{\"a\":\"baz\",\"b\":\"6\"}]"))
})