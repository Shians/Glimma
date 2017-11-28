#' Numeric to hex colour converter
#'
#' Convert numbers and R colour strings into corresponding hex codes for colours
#'
#' @param x the colour value(s) to be converted to hex values.
#'
#' @return hex codes for colours
#'
#' @examples
#' as.hexcol(c(1, 2, 3))
#' as.hexcol(c("red", "black", "green"))
#'
#' @importFrom grDevices palette col2rgb
#'
#' @export

as.hexcol <- function(x) {
    charToHexCol <- function(x) {
        requireNamespace("grDevices")
        temp <- as.character(as.hexmode(col2rgb(x, alpha=FALSE)))
        out <- apply(temp, 2, function(x) {paste0("#", paste0(x, collapse=""))})

        sapply(out, function(x) { ifelse(x=="#000", "#000000", x) })
    }

    numToHexCol <- function(x) {
        requireNamespace("grDevices")
        x <- as.integer(x)

        output <- "#000000"

        if (all(x > 0)) {
            output <- as.character(charToHexCol(palette()[x]))
        } else {
            output[x > 0] <- as.character(charToHexCol(palette()[x[x > 0]]))
            warning("0 is not a valid colour selection.")
        }

        output
    }

    output <- ""

    if (is.character(x)) {
        if (all(is.hex(x))) {
            output <- x
        } else {
            temp <- x
            temp[!is.hex(x)] <- charToHexCol(temp[!is.hex(x)])
            output <- temp
        }
    } else if (is.numeric(x)) {
        output <- numToHexCol(x)
    } else {
        warning("input is not character or numeric, no hex conversion made.")
        output <- x
    }

    return(output)
}

#' Hexcode colours
#'
#' Check if string(s) are valid hex colour representation
#'
#' @param x the colour value(s) to check.
#'
#' @return Logical vector indicating if strings(s) are valid hex representations

is.hex <- function(x) {
    isHex <- grepl("^#[[:xdigit:]]{6}$", x)
    isHexWithAlpha <- grepl("^#[[:xdigit:]]{8}$", x)

    return(isHex | isHexWithAlpha)
}
