# Function to print stored json as a javascript var declaration
printJsonToFile <- function(json, filename, varname) {
    file.con <- file(description=filename, open="w")

    if (length(json) != length(varname)) {
        stop("json vector must be same length as varname vector")
    }

    for (i in 1:length(json)) {
        write(paste0("var ", varname[i], " = ", json[i], ";"), file=file.con,
                    sep=" ", append=TRUE)
    }

    close(file.con)
}

# Function to get the nth character of a string
char <- function(string, n) {
    checkThat(string, isString)

    if (any(abs(n) > nchar(string))) {
        stop(paste(quotify("n"), "is outside index range"))
    }

    if (n == 0) {
        return(rep("", length(string)))
    }

    if (n < 0) {
        n <- nchar(string) + n + 1
    }

    return(substr(string, n, n))
}

# Function to return filepaths
pathMaker <- function(path) {
    if (char(path, -1) != "/") {
        stop("path must end with /")
    }
    function (x) {
        return(paste0(path, x))
    }
}

# String to hex colour converter


CharToHexCol <- function(x) {
    requireNamespace("grDevices")
    temp <- as.character(as.hexmode(col2rgb(x, alpha=FALSE)))
    out <- apply(temp, 2, function(x) {paste0("#", paste0(x, collapse=""))})
    sapply(out, function(x) { ifelse(x=="#000", "#000000", x) })
}

# Numeric to hex colour converter

NumToHexCol <- function(x) {
    requireNamespace("grDevices")
    x <- palette()[as.integer(x)]
    temp <- as.character(as.hexmode(col2rgb(x, alpha=FALSE)))
    out <- apply(temp, 2, function(x) {paste0("#", paste0(x, collapse=""))})
    sapply(out, function(x) { ifelse(x=="#000", "#000000", x) })
}

# Function to check if values are valid hexadecimal expressions
is.hex <- function(x) {
    (grepl("#[[:xdigit:]]{6}", x) | grepl("#[[:xdigit:]]{8}", x))
}

#' Numeric to hex colour converter
#'
#' Functions to convert numbers into corresponding hex codes for colours
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
    if (is.character(x)) {
        if (all(is.hex(x))) {
            return(x)
        } else {
            temp <- x
            temp[!is.hex(x)] <- CharToHexCol(temp[!is.hex(x)])
            return(temp)
        }
    } else if (is.numeric(x)) {
        return(NumToHexCol(x))
    } else {
        warning("input is not character or numeric, no hex conversion made.")
        return(x)
    }
}

rmDuplicateCols <- function(x) {
    x[, !duplicated(names(x))]
}
