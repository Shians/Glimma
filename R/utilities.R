# Function to print stored json as a javascript var declaration
printJsonToFile <- function(json, filename, varname) {
    file.con <- file(description=filename, open="w")

    if (length(json) != length(varname)) {
        stop("json vector must be same length as varname vector")
    }

    for (i in seq_along(json)) {
        write(paste0("var ", varname[i], " = ", json[i], ";"), file=file.con,
                    sep=" ", append=TRUE)
    }

    close(file.con)
}

# function to return filepaths
pathMaker <- function(path) {
    if (char(path, -1) != "/") {
        stop("path must end with /")
    }

    return ( function (x) { return(paste0(path, x)) } )
}

# removes columns with the same name
rmDuplicateCols <- function(x) {
    x[, !duplicated(names(x))]
}

# appends enumeration to duplicated values in vector
makeUnique <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "NA"

    dupes <- x[duplicated(x)]
    dupes <- unique(dupes)

    for(d in dupes) {
        dupe_ind <- x == d
        n_dupe <- sum(dupe_ind)
        x[dupe_ind] <- paste(x[dupe_ind], 1:n_dupe, sep=".")
    }

    x
}

# not in operator
"%!in%" <- function(x, y) !("%in%"(x, y))

# get row of matrix
getRows <- function(x, inds) {
    x[inds, , drop=FALSE]
}

# get column of matrix
getCols <- function(x, inds) {
    x[, inds, drop=FALSE]
}

# not null check
not.null <- function(x) {
    !is.null(x)
}

# sequence along 1:nrow(x)
seq_rows <- function(x) {
    seq_len(nrow(x))
}

# sequence along 1:ncol(x)
seq_cols <- function(x) {
    seq_len(ncol(x))
}