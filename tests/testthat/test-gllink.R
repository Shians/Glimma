context("Chart linking functions")

test_that("Linking functions are correct", {
    expect_error(gllink(0, 1, src="none", dest="none", flag="none"),
        "'src', 'dest' and 'flag' cannot simultaneously be 'none'")
    expect_error(gllink(0, 1, src="click", dest="none"),
        "src cannot be defined while dest is 'none'")
    expect_error(gllink(0, 1, src="none", dest="click"),
        "dest cannot be defined while src is 'none'")

    expected_link <- list()
    expected_link$link <- data.frame(from=0, to=1, src="click", dest="click", flag="none", info="none")
    expected_link$type <- "link"
    class(expected_link) <- "jslink"

    expect_equal(gllink(from=0, to=1, src="click", dest="click"), expected_link)
})
