checkThat <- function(x, fun=NULL, type=c("error", "warning"), msg=NULL) {
    type <- match.arg(type)

    errMsg <- fun(x)
    output <- TRUE

    if (errMsg != "") {
        if (not.null(msg)) {
            errMsg <- msg
        }
        if (type == "error") {
            stop(errMsg)
        } else if (type == "warning") {
            warning(errMsg)
        }
        output <- FALSE
    }

    invisible(output)
}

hasCols <- function(...) {
    colNames <- unlist(list(...))

    outputFun <- function(x) {
        x <- data.frame(x)
        cond <- sapply(colNames, function(d) { d %in% colnames(x)})

        output <- ""

        if (any(!cond)) {
            missingCols <- names(cond)[which(!cond)]
            missingCols <- paste(quotify(missingCols), collapse=", ")
            errMsg <- paste("Columns:", missingCols, "not found")

            output <- errMsg
        }

        output
    }

    outputFun
}

hasRows <- function(...) {
    rowNames <- unlist(list(...))

    outputFun <- function(x) {
        x <- data.frame(x)
        cond <- sapply(rowNames, function(d) { d %in% rownames(x)})

        output <- ""

        if (any(!cond)) {
            missingRows <- names(cond)[which(!cond)]
            errMsg <- paste("Rows:",
                                paste(quotify(missingRows), collapse=", "),
                                "not found")
            output <- errMsg
        }

        output
    }

    outputFun
}

isType <- function(x, typename, typecheck=NULL) {
    if (is.null(typecheck)) {
        typecheck <- function(x) {
            output <- class(x) == typename

            output
        }
    }

    cond <- all(typecheck(x))

    output <- ""

    if (!cond) {
        errMsg <- paste("Argument must be of type", typename)

        output <- errMsg
    }

    output
}

isClass <- function(typename) {
    output <- function(x) {
        isType(x, typename)
    }

    output
}

isString <- function(x) {
    isType(x, "character", is.character)
}

isCharacter <- isString

isNumeric <- function(x) {
    isType(x, "numeric", is.numeric)
}

isLogical <- function(x) {
    isType(x, "logical", is.logical)
}

isFactor <- function(x) {
    isType(x, "factor", is.factor)
}

isUnique <- function(x) {
    cond <- !anyDuplicated(x)

    output <- ""

    if (!cond) {
        errMsg <- paste("Argument expected to contain unique values")

        output <- errMsg
    }

    output
}

hasLength <- function(n) {
    outputFun <- function(x) {
        cond <- length(x) == n

        output <- ""

        if (!cond) {
            errMsg <- paste("Argument should have length", n)
            errMsg <- paste0(errMsg, ",")
            errMsg <- paste(errMsg, "instead has length", length(x))

            output <- errMsg
        }

        output
    }

    outputFun
}

sameAs <- function(b) {
    outputFun <- function(a) {
        cond <- a == b

        output <- ""

        if (!cond) {
            errMsg <- "Arguments should be same value,"
            errMsg <- paste(errMsg, "instead found", a, "vs", b)

            output <- errMsg
        }

        output
    }

    outputFun
}

isIn <- function(b) {
    outputFun <- function(a) {
        cond <- a %in% b

        output <- ""

         if (!cond) {
            errMsg <- "Second argument should contain the first."

            output <- errMsg
        }

        output
    }

    outputFun
}

notNull <- function(x) {
    cond <- is.null(x)

    output <- ""

    if (cond) {
        errMsg <- paste("Argument must not be NULL")

        output <- errMsg
    }

    output
}
