context("Test as.hexcol")

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

    expect_warning(as.hexcol(0))
    expect_equal(as.hexcol(1), "#000000")
    expect_equal(as.hexcol(2), "#ff0000")
})
