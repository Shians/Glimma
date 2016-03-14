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

# Function to add double quotes onto the start and end of the strings
quotify <- function(x) {
    paste("\"", x, "\"", sep="")
}

# Function to add square brackets around string
arrayify <- function(x) {
    paste("[", paste(x, collapse=","), "]", sep="")
}

# Function to add braces around string
objectify <- function(x, y) {
    paste("{", paste(x, y, sep=":", collapse=","), "}", sep="")
}

#' JSON converter for data frames
#' 
#' Function to create a JSON from a data.frame
#' 
#' @param df the data.frame to be converted into JSON
#' 
#' @return a stringified JSON, the data.frame is encoded as a vector of objects, 
#' with each column being one object with keys corresponding to column names.
#' 
#' @importFrom methods is

makeDFJson <- function(df) {
    df <- data.frame(df)

    for (n in names(df)) {
        if (!is(df[[n]], "numeric") && !is(df[[n]], "logical")) {
            df[[n]] <- quotify(as.character(df[[n]]))
        }
    }

    coln <- paste(quotify(colnames(df)), ":", sep="")
    temp <- t(apply(df, 1, function (x) { paste(coln, ifelse(is.na(x), "\"NA\"", x), sep="") }))
    temp <- apply(temp, 1, function (x) { paste("{", paste(x, collapse=","), "}", sep="") })

    output <- paste("[", paste(temp, collapse=","), "]", sep="")
    class(output) <- "json"

    output
}

#' JSON converter for chart objects
#' 
#' Function to make json object from a chart, ignoring the json property
#' 
#' @param chart the chart object to be converted into JSON
#' 
#' @return a stringified JSON object containing the chart data.
#' 
#' @importFrom methods is

makeChartJson <- function(chart) {
    if (!is(chart, "jschart")) {
        stopType("jschart", "chart")
    }

    makeEntry <- function(d) {
        if (is.null(chart[[d]])) {
            return(paste(quotify(d), "null", sep=":"))
        } else if (length(chart[[d]]) > 1) {
            return(paste(quotify(d), arrayify(quotify(chart[[d]])), sep=":"))
        } else if (is.list(chart[[d]])) {
            return(paste(quotify(d), makeListJson(chart[[d]]), sep=":"))
        } else {
            return(paste(quotify(d), quotify(chart[[d]]), sep=":"))
        }
    }

    chart$json <- NULL

    paste0("{", paste(sapply(names(chart), makeEntry), collapse=","), "}")
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

# Function to make json object ouf of factor levels
makeFactJson <- function(sample.groups) {
    sample.groups <- as.factor(sample.groups)
    l <- levels(sample.groups)
    l <- paste("\"", l, "\"", sep="")
    paste("[", paste(l, collapse=", "), "]", sep="")
}

# Function to make json object out of a list
makeListJson <- function(x) {
    if (!is.list(x)) {
        stopType("list", "x")
    }

    parse <- function(d) { 
        if (is.numeric(x[[d]])) {
            paste(quotify(d), x[[d]], sep=":")
        } else {
            paste(quotify(d), quotify(x[[d]]), sep=":")
        }
    }

    keys <- names(x)
    entries <- sapply(keys, parse)
    output <- paste("{", paste(entries, collapse=","), "}", sep="")

    class(output) <- "json"

    output
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

# 
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
