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
    vals <- sapply(x, function(d) { jsonlite::toJSON(d, auto_unbox=TRUE, na="string", use_signif=TRUE) })

    output <- paste(paste(quotify(list_names), vals, sep=":"), collapse=",")

    paste("{", output, "}", sep="")
}

#' JSON converter for data frames
#'
#' Function to create a JSON from a data.frame
#'
#' @param df the data.frame to be converted into JSON
#' @param convert.logical whether to convert logicals into strings "TRUE" and "FALSE"
#' @param dataframe how to encode data.frame objects: must be one of 'rows', 'columns'
#'
#' @return a stringified JSON, the data.frame is encoded as a vector of objects,
#' with each column being one object with keys corresponding to column names.
#'
#' @importFrom methods is

makeJson.data.frame <- function(
    df,
    convert.logical = TRUE,
    dataframe = c("rows", "columns")
) {
    dataframe <- match.arg(dataframe)

    if (convert.logical) {
        logicalCols <- getLogicalCols(df)

        for (i in which(logicalCols)) {
            df[, i] <- as.character(df[, i])
        }
    }

    # don't need rownames in our JSON object
    rownames(df) <- NULL
    output <- jsonlite::toJSON(
        df,
        auto_unbox = TRUE,
        na = "string",
        use_signif = TRUE,
        dataframe = dataframe
    )
    class(output) <- "json"

    output
}

makeJson.default <- function(x, ...) {
    jsonlite::toJSON(x, auto_unbox=TRUE, use_signif=TRUE, na="string")
}

makeJson.character <- function(x, ...) {
    jsonlite::toJSON(x, auto_unbox=TRUE, na="string")
}

makeJson.numeric <- function(x, ...) {
    jsonlite::toJSON(x, auto_unbox=TRUE, use_signif = TRUE, na="string")
}

makeJson.logical <- function(x, ...) {
    jsonlite::toJSON(x, auto_unbox=TRUE, na="string")
}

makeJson.NULL <- function(x, ...) {
    jsonlite::toJSON(rep("null", length(x)))
}

# Function to add square brackets around string
arrayify <- function(x) {
    paste("[", paste(x, collapse=","), "]", sep="")
}

getLogicalCols <- function(df) {
    logicalCols <- logical(ncol(df))
    for (i in seq_cols(df)) {
        logicalCols[i] <- is.logical(df[, i])
    }

    logicalCols
}
