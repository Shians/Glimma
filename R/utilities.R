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

    return ( function (x) { return(paste0(path, x)) } )
}

rmDuplicateCols <- function(x) {
    x[, !duplicated(names(x))]
}
