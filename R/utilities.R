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

makeUnique <- function(x) {
    x <- as.character(x)

    dupes <- x[duplicated(x)]
    dupes <- unique(dupes)

    for(d in dupes) {
        dupe_ind <- x == d
        n_dupe <- sum(dupe_ind)
        x[dupe_ind] <- paste(x[dupe_ind], 1:n_dupe, sep=".")
    }

    x
}

"%!in%" <- function(x, y)!("%in%"(x, y))

getRows <- function(x, inds) {
    x[inds, , drop=FALSE]
}

getCols <- function(x, inds) {
    x[, inds, drop=FALSE]
}

not.null <- function(x) {
    !is.null(x)
}
