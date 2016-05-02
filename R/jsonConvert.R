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

makeJson.chart <- function(chart) {
    if (!is(chart, "jschart")) {
        stopType("jschart", "chart")
    }

    makeEntry <- function(d) {
        return(paste(quotify(d), makeJson(chart[[d]]), sep=":"))
    }

    chart$json <- NULL

    paste0("{", paste(sapply(names(chart), makeEntry), collapse=","), "}")
}

# Function to make json object out of factor levels
makeJson.factor <- function(sample.groups) {
    sample.groups <- as.factor(sample.groups)
    l <- levels(sample.groups)
    l <- paste("\"", l, "\"", sep="")
    paste("[", paste(l, collapse=", "), "]", sep="")
}

# Function to make json object out of a lists
makeJson.list <- function(x) {
    if (!is.list(x)) {
        stopType("list", "x")
    }

    parse <- function(d) {
        paste(quotify(d), makeJson(x[[d]]), sep=":")
    }

    keys <- names(x)
    entries <- sapply(keys, parse)
    output <- paste("{", paste(entries, collapse=","), "}", sep="")

    class(output) <- "json"

    output
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

makeJson.data.frame <- function(df) {
    df <- data.frame(df)

    for (n in names(df)) {
        if (!is(df[[n]], "numeric") && !is(df[[n]], "logical")) {
            df[[n]] <- quotify(as.character(df[[n]]))
        }
    }

    f1 <- function (x) { paste(coln, ifelse(is.na(x), "\"NA\"", x), sep="") }
    f2 <- function (x) { paste("{", paste(x, collapse=","), "}", sep="") }
    coln <- paste(quotify(colnames(df)), ":", sep="")
    temp <- t(apply(df, 1, f1))
    temp <- apply(temp, 1, f2)

    output <- paste("[", paste(temp, collapse=","), "]", sep="")
    class(output) <- "json"

    # Outputs [{"col1": val1.1, "col2": val1.2,...},
    #          {"col1": val2.1, "col2": val2.2,...},
    #          ...]
    output
}

makeJson.character <- function(x, ...) {
    if (length(x) > 1) {
        arrayify(quotify(x))
    } else {
        quotify(x)
    }
}

makeJson.numeric <- function(x, ...) {
    if (length(x) > 1) {
        arrayify(x)
    } else {
        x
    }
}

makeJson.logical <- function(x, ...) {
    if (length(x) > 1) {
        # Convert from R logicals to Javascript logicals
        arrayify(c("false", "true")[x+1])
    } else {
        c("false", "true")[x+1]
    }
}

makeJson.NULL <- function(x, ...) {
    if (length(x) > 1) {
        arrayify(rep("null", length(x)))
    } else {
        "null"
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

# For legacy function compatibility
makeChartJson <- makeJson.chart
makeFactJson <- makeJson.factor
makeListJson <- makeJson.list
makeDFJson <- makeJson.data.frame
