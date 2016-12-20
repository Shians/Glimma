# Function to add double quotes onto the start and end of the strings
quotify <- function(x) {
    paste("\"", x, "\"", sep="")
}

# Function to get the nth character of a string
char <- function(string, n) {
    checkThat(string, isString)

    if (any(abs(n) > nchar(string))) {
        stop(paste(quotify(n), "is outside index range"))
    }

    if (n == 0) {
        return("")
    }

    if (n < 0) {
        n <- nchar(string) + n + 1
    }

    return(substr(string, n, n))
}

lastChar <- function(string) {
    char(string, nchar(string))
}

firstChar <- function(string) {
    char(string, 1)
}
