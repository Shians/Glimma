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
    args <- unlist(list(...))

    checkThat(args, isCharacter)

    length.fail <- !all(sapply(args, length) == 1)
    if (length.fail) {
        stop("all arguments must be singular strings")
    }

    # Output func1.func2.func3...
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
    checkThat(args, isCharacter)

    paste(args, collapse=", ")
}

# Function to write a javascript method call
jsCall <- function(func, arg) {
    checkThat(func, isClass("jsMethod"))

    if (is(arg, "jsCall")) {
        arg <- gsub(";\n", "", arg)
    }
    # Output func(arg1, arg2, ...)
    out <- paste0(func, paste0("(", arg, ");\n"))
    class(out) <- "jsCall"

    out
} # Vectorise in the future?

# Function to write chained javascript calls
jsChain <- function(...) {
    args <- list(...)

    sapply(args, function(x) { checkThat(x, isClass("jsCall")) })

    trimmed <- gsub(";\n", "", as.character(unlist(args)))
    chained <- paste(trimmed, collapse=".")

    # func1(); func2(); -> func1().func2()
    out <- paste0(chained, ";\n")
    class(out) <- "jsCall"

    out
} # Vectorise in the future?
