#' JSON converter for R objects
#'
#' Function to generate json strings from
#'
#' @param x the object to be converted into JSON
#' @param ... additional arguments
#'
#' @return a stringified JSON object.
#'
#' @importFrom jsonlite toJSON
makeJson <- function(x, ...) {
    UseMethod("makeJson")
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

makeJson.jschart <- function(chart) {
    makeEntry <- function(d) {
        return(paste(quotify(d), makeJson(chart[[d]]), sep=":"))
    }

    chart$json <- NULL

    columnNames <- names(chart)
    paste0("{", paste(sapply(columnNames, makeEntry), collapse=","), "}")
}

# Function to make json object out of factor levels
makeJson.factor <- function(sample.groups) {
    l <- levels(sample.groups)
    l <- paste("\"", l, "\"", sep="")
    output <- paste("[", paste(l, collapse=","), "]", sep="")

    class(output) <- "json"

    output
}

# Function to make json object out of a lists
makeJson.list <- function(x, ...) {
    list_names <- names(x)
    vals <- sapply(x, function(d) { toJSON(d, auto_unbox=TRUE, na="string") })
    output <- paste(paste(quotify(list_names), vals, sep=":"), collapse=",")

    paste("{", output, "}", sep="")
}

#' JSON converter for data frames
#'
#' Function to create a JSON from a data.frame
#'
#' @param df the data.frame to be converted into JSON
#' @param convert.logical whether to convert logicals into strings "TRUE" and "FALSE"
#'
#' @return a stringified JSON, the data.frame is encoded as a vector of objects,
#' with each column being one object with keys corresponding to column names.
#'
#' @importFrom methods is

makeJson.data.frame <- function(df, convert.logical=TRUE) {
    if (convert.logical) {
        logicalCols <- getLogicalCols(df)

        for (i in which(logicalCols)) {
            df[, i] <- convertLogical(df[, i])
        }
    }

    output <- toJSON(df, auto_unbox=TRUE, na="string")
    class(output) <- "json"

    # Outputs [{"col1": val1.1, "col2": val1.2,...},
    #          {"col1": val2.1, "col2": val2.2,...},
    #          ...]
    output
}

convertLogical <- function(x) {
    as.character(x)
}

makeJson.character <- function(x, ...) {
    toJSON(x, auto_unbox=TRUE, na="string")
}

makeJson.numeric <- function(x, ...) {
    toJSON(x, auto_unbox=TRUE, na="string")
}

makeJson.logical <- function(x, ...) {
    toJSON(x, auto_unbox=TRUE, na="string")
}

makeJson.NULL <- function(x, ...) {
    toJSON(rep("null", length(x)))
}

# Function to add square brackets around string
arrayify <- function(x) {
    paste("[", paste(x, collapse=","), "]", sep="")
}

# Function to add braces around string
objectify <- function(x, y) {
    paste("{", paste(x, y, sep=":", collapse=","), "}", sep="")
}

notNumericOrLogical <- function(x) {
    !is(x, "numeric") && !is(x, "logical")
}

getLogicalCols <- function(df) {
    logicalCols <- logical(ncol(df))
    for (i in 1:ncol(df)) {
        logicalCols[i] <- is.logical(df[, i])
    }

    logicalCols
}
