##
# Helper functions for writing javascript

writeMaker <- function(filename) {
    function (string) {
        f <- cat
        formals(f)$file <- filename
        formals(f)$append <- TRUE
        f(string)
    }
}

# Function to write the opening tag for BootStrap.js row
bsRowStart <- function() {
    writeMaker("<div class=\"row\">")
}

# Function to write the closing tag for BootStrap.js row
bsRowEnd <- function() {
    writeMaker("</div>")
}

# Function to write a column cell for BootStrap.js
bsCol <- function(size, class="plot-device", type="md") {
    col.type <- quotify(paste("col", size, type, sep="-"))
    tag <- paste0("<div class=", col.type, "></div>")
    writeMaker(tag)
}

# Function to write javascript methods
jsMethod <- function(...) {
    args <- list(...)

    class.fail <- !all(sapply(args, class) == "character")
    if (class.fail) {
        stop("all arguments must be of character class")
    }

    length.fail <- !all(sapply(args, length) == 1)
    if (length.fail) {
        stop("all arguments must be singular strings")
    }

    # Output arg1.arg2.arg3...
    out <- paste(unlist(args), collapse=".")
    class(out) <- "jsMethod"

    out
}

# Function to write javascript functions
jsFunction <- function(func) {
    if (length(func) != 1) {
        stop("'func' must be a singular string")
    }

    jsMethod(func)
}

# Function to write javascript arguments
jsArgs <- function(...) {
    args <- unlist(list(...))
    class.fail.args <- !is(args, "character")
    if (class.fail.args) {
        stop("'args' must be of character class")
    }

    paste(args, collapse=", ")
}

# Function to write a javascript method call
jsCall <- function(func, args) {
    class.fail.func <- !is(func, "jsMethod")
    if (class.fail.func) {
        stop("'func' must be of jsMethod class")
    }

    # Output func(arg1, arg2, ...)
    out <- paste0(func, paste0("(", args, ");\n"))
    class(out) <- "jsCall"

    out
} # Vectorise in the future?

# Function to write chained javascript calls
jsChain <- function(...) {
    args <- list(...)

    class.fail <- !all(sapply(args, class) == "jsCall")
    if (class.fail) {
        stop("all arguments must be of jsCall class")
    }

    trimmed <- gsub(";\n", "", unlist(args))
    chained <- paste(trimmed, collapse=".")

    paste0(chained, ";\n")
} # Vectorise in the future?
