# converts status vector to colours
convertStatusToCols <- function(x, cols) {

    isDefaultStates <- function(x) all(x == -1 | x == 0 | x == 1)
    isDefaultCols <- function(x) identical(x, c("#00bfff", "#858585", "#ff3030"))

    if (isDefaultStates(x) && isDefaultCols(cols)) {
        # using default states and colours
        x <- factor(x, levels=c(-1, 0, 1))
    } else {
        # if custom states or colours used
        uniqueStates <- length(unique(x))

        if (uniqueStates != length(cols)) {
            stop("The number of status levels differ from number of supplied colours. ",
                    uniqueStates, " vs ", length(cols), ".")
        }

        status_levels <- sort(unique(x))
        # relevel the input factor and colours such that zero colours are moved to the
        # front, this causes them to be draw first and allows the remaining points to be
        # drawn on top.
        cols <- c(cols[status_levels==0], cols[status_levels!=0])
        cols_levels <- c(0, setdiff(status_levels, 0))
        x <- factor(x, levels=cols_levels)
    }

    if (all(x == 0)) {
        # all insignificant
        output <- rep(cols[2], length(x))
    } else {
        output <- cols[x]
    }

    output
}

#TODO: Add test
makeAnno <- function(x, anno) {
    output <- NULL
    if (is.null(anno) && is.null(x$genes)) {

        warning("No gene annotation provided.")

    } else if (not.null(anno) && not.null(x$genes)) {

        anno <- cbind(anno, x$genes)
        anno <- rmDuplicateCols(anno)

    } else if (not.null(x$genes)) {

        anno <- x$genes

    }

    output <- anno

    output
}

#TODO: Add test
setDisplayColumns <- function(display.columns, anno, xval, yval) {
    if (is.null(display.columns)) {
        display.columns <- names(anno)
        display.columns <- display.columns[display.columns != xval]
        display.columns <- display.columns[display.columns != yval]
    }

    output <- make.names(display.columns)

    output
}

# assigns groups as 1:n
initialiseGroups <- function(n) {
    output <- NULL
    if (not.null(n)) {
        output <- 1:n
    }

    output
}

# reorder rows so the background colours are at the top
sortInsigPointsToTop <- function(plotting.data, bg.col) {
    output <- rbind(getRows(plotting.data, plotting.data$cols == bg.col),
                    getRows(plotting.data, plotting.data$cols != bg.col))

    output
}

#TODO: Add test
transformCounts <- function(counts, transform, colnames=colnames(counts)) {
    rownames(counts) <- make.names(colnames)

    if (transform) {
        output <- as.matrix(edgeR::cpm(counts, log=TRUE))
    } else {
        output <- as.matrix(counts)
    }

    output <- t(output)

    output
}

#TODO: Add test
checkCountsAndSamples <- function(counts, samples, side.log=FALSE) {
    if (not.null(counts)) {
        if (not.null(samples)) {
            checkThat(ncol(counts), sameAs(length(samples)))
        }

        if (side.log && any(counts == 0)) {
            stop("Zeros in expression matrix cannot be plotted on log-scale, consider adding small offset.")
        }
    }
}

#TODO: Add test
naRowInds <- function(res.df, ...) {
    res.df <- data.frame(res.df)
    filterCols <- unlist(list(...))

    delRows <- rep(FALSE, nrow(res.df))

    for (cols in filterCols) {
        delRows <- delRows | is.na(res.df[, cols])
    }

    delRows
}

#TODO: Add test
checkObjAnnoCountsShapes <- function(anno, counts, x) {
    if (not.null(anno)) {
        checkThat(nrow(anno), notNull)
        checkThat(nrow(x), notNull)

        checkThat(nrow(anno), sameAs(nrow(x)))
    }

    if (not.null(counts)) {
        checkThat(nrow(counts), notNull)
        if (not.null(anno)) {
            checkThat(nrow(anno), notNull)
            checkThat(nrow(counts), sameAs(nrow(anno)))
        }
    }
}

#TODO: Add test
checkSideMainPresent <- function(side.main, anno, x) {
    if (class(x) == "DGELRT" || class(x) == "DGEExact") {
        if (side.main %!in% union(colnames(anno), colnames(x$table))) {
            stop(paste("column", quotify(side.main), "cannot be found in x$table or anno."))
        }
        if (not.null(x$table)) {
            combined_anno <- cbind(anno, x$table)
        } else {
            combined_anno <- anno
        }
    } else if (class(x) == "MArrayLM") {
        if (side.main %!in% union(colnames(anno), colnames(x$genes))) {
            stop(paste("column", quotify(side.main), "cannot be found in x$genes or anno."))
        }
        if (not.null(x$genes)) {
            combined_anno <- cbind(anno, x$genes)
        } else {
            combined_anno <- anno
        }
    } else if (class(x) == "DESeqResults") {
        if (side.main %!in% union(colnames(anno), names(x@listData))) {
            stop(paste("column", quotify(side.main), "cannot be found in x or anno."))
        }
        if (not.null(x@listData)) {
            combined_anno <- cbind(anno, x@listData)
        } else {
            combined_anno <- anno
        }
    } else {
        if (side.main %!in% union(colnames(anno), colnames(x))) {
            stop(paste("column", quotify(side.main), "cannot be found in x or anno."))
        }
        if (is.data.frame(x)) {
            combined_anno <- cbind(anno, x)
        } else {
            combined_anno <- anno
        }
    }
}

#' glMDPlot Rmarkdown link and instructions
#'
#' When run inside of a text-block of Rmarkdown document using `r ...` this
#' produces a link and instructions about the usage of the interactive plots.
#'
#' @param html name of the HTML page containing plots from glMDPlot.
#'
#' @seealso \code{\link{glMDPlot}}
#'
#' @return None
#'
#' @examples
#' glMDRmd()
#'
#' @export
glMDRmd <- function(html = "MD-Plot") {
    paste(c(
		paste0("[Click here for interactive version]",
              "(", "glimma-plots/", html , ".html", ")"),
        "",
        "* Hover over points to see sample-wise expression",
        "* Click on column names to sort by column",
        "* Click rows on tables to highlight gene"
    ), collapse = "\n")
}
