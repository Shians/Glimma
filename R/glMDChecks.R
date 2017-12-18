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

# check that anno has the same rows as counts has columns
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

# check that side.main exists as a column in either anno or main object
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