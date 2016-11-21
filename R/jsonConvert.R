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

    paste0("{", paste(sapply(names(chart), makeEntry), collapse=","), "}")
}

# Function to make json object out of factor levels
makeJson.factor <- function(sample.groups) {
    sample.groups <- as.factor(sample.groups)
    l <- levels(sample.groups)
    l <- paste("\"", l, "\"", sep="")
    paste("[", paste(l, collapse=","), "]", sep="")

    class(output) <- "json"
}

# Function to make json object out of a lists
makeJson.list <- function(x) {
    checkThat(x, isClass("list"))

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
#' @param convert.logical whether to convert logicals into strings "TRUE" and "FALSE"
#'
#' @return a stringified JSON, the data.frame is encoded as a vector of objects,
#' with each column being one object with keys corresponding to column names.
#'
#' @importFrom methods is

makeJson.data.frame <- function(df, convert.logical=TRUE) {
    df <- data.frame(df)

    for (n in names(df)) {
        df[[n]] <- convertLogical(df[[n]], convert.logical)
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

convertLogical <- function(x, convert.logical) {
    output <- x

    if (!is(x, "numeric") && !is(x, "logical")) {
        output <- quotify(as.character(x))
    } else if (is(x, "logical")) {
        if (convert.logical) {
            output <- quotify(as.character(x))
        } else {
            output <- makeJson(x)
        }
    }
    class(output) <- "json"

    output
}

makeJson.character <- function(x, ...) {
    if (length(x) > 1) {
        output <- arrayify(quotify(x))
    } else {
        output <- quotify(x)
    }
    class(output) <- "json"

    output
}

makeJson.numeric <- function(x, ...) {
    if (length(x) > 1) {
        output <- arrayify(x)
    } else {
        output <- x
    }

    output
}

makeJson.logical <- function(x, ...) {
    if (length(x) > 1) {
        # Convert from R logicals to Javascript logicals
        output <- arrayify(c("false", "true")[x+1])
    } else {
        output <- c("false", "true")[x+1]
    }

    output
}

makeJson.NULL <- function(x, ...) {
    if (length(x) > 1) {
        output <- arrayify(rep("null", length(x)))
    } else {
        output <- "null"
    }

    output
}

# Function to add square brackets around string
arrayify <- function(x) {
    paste("[", paste(x, collapse=","), "]", sep="")
}

# Function to add braces around string
objectify <- function(x, y) {
    paste("{", paste(x, y, sep=":", collapse=","), "}", sep="")
}
