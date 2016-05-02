# Helper function for printing type errors
stopType <- function(type, name="x") {
    type <- quotify(type)
    name <- quotify(name)
    stop(paste("input", name, "not of class", type))
}

# Helper function for asserting object class
assertClass <- function(x, type) {
    if (!is(x, type)) {
        arg.name <- deparse(substitute(x))
        stopType(type, arg.name)
    }
}

# Helper function for asserting list classes
lassertClass <- function(x, type) {
    check.fail <- !all(lapply(x, class) == type)
    if (check.fail) {
        name <- quotify(deparse(substitute(x)))
        type <- quotify(type)
        stop(paste("at least one member of list", name, "not of class", type))
    }
}

#' Column checker
#'
#' Check if data.frame controls all the listed columns
#'
#' @param df the data frame to check.
#' @param columns the columns that should exist in the data frame.
#'
#' @return stops program with an error if column cannot be found in df

hasColumns <- function(df, columns) {
    if (!all(columns %in% names(df))) {
        violations <- paste(quotify(columns[columns %in% names(df)]), collaps=", ")
        stop(paste(violations, "cannot be found in", deparse(substitute(df))))
    }
}

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
    if (!is(string, "character")) {
        stopType("string", "character")
    }

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

#' String to hex colour converter
#'
#' Function to convert colour strings into hex codes
#'
#' @param x the string colour value(s) to be converted to hex values.
#'
#' @return hex codes for colours
#'
#' @importFrom grDevices col2rgb

CharToHexCol <- function(x) {
    temp <- as.character(as.hexmode(col2rgb(x, alpha=FALSE)))
    out <- apply(temp, 2, function(x) {paste0("#", paste0(x, collapse=""))})
    sapply(out, function(x) { ifelse(x=="#000", "#000000", x) })
}

#' Numeric to hex colour converter
#'
#' Functions to convert numbers into corresponding hex codes for colours
#'
#' @param x the colour value(s) to be converted to hex values.
#'
#' @return hex codes for colours
#'
#' @importFrom grDevices palette col2rgb

NumToHexCol <- function(x) {
    col <- palette()[as.integer(col)]
    temp <- as.character(as.hexmode(col2rgb(x, alpha=FALSE)))
    out <- apply(temp, 2, function(x) {paste0("#", paste0(x, collapse=""))})
    sapply(out, function(x) { ifelse(x=="#000", "#000000", x) })
}

# Function to check if values are valid hexadecimal expressions
is.hex <- function(x) {
    (grepl("#[[:xdigit:]]{6}", x) | grepl("#[[:xdigit:]]{8}", x))
}

# Function to convert colours or numbers to hex colour values
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
