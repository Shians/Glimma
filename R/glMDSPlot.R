#' Glimma MDS Plot
#'
#' @template desc_glMDSPlot
#'
#' @author Shian Su, Gordon Smyth
#'
#' @param x the matrix containing the gene expressions.
#' @param ... additional arguments.
#'
#' @template return_glMDSPlot
#'
#' @seealso \code{\link{glMDSPlot.default}}, \code{\link{glMDSPlot.DGEList}}
#'
#' @examples
#' data(lymphomaRNAseq)
#' genotype <- relevel(lymphomaRNAseq$samples$group, "Smchd1-null")
#' \donttest{
#' glMDSPlot(lymphomaRNAseq, labels=1:7, groups=genotype)
#' }
#'
#' @export

glMDSPlot <- function(x, ...) {
    UseMethod("glMDSPlot")
}

#' Glimma MDS Plot
#'
#' @template desc_glMDSPlot
#'
#' @author Shian Su, Gordon Smyth
#'
#' @inheritParams glMDSPlot
#' @template params_glMDSPlot
#'
#' @template return_glMDSPlot
#'
#' @method glMDSPlot default
#'
#' @importFrom stats cmdscale as.dist
#'
#' @export

# Code taken from plotMDS of limma bioConductor package with alterations
glMDSPlot.default <- function(
    x,
    top = 500,
    labels = seq_cols(x),
    groups = rep(1, ncol(x)),
    gene.selection = c("pairwise", "common"),
    main  ="MDS Plot",
    path = getwd(),
    folder = "glimma-plots",
    html = "MDS-Plot",
    launch = TRUE,
    ...
) {
    #   Multi-dimensional scaling with top-distance
    #   Di Wu and Gordon Smyth
    #   19 March 2009.  Last modified 14 Jan 2015
    #   Modified by Shian Su on 25 Jan 2016

    ##
    # Check Inputs

    x <- as.matrix(x)
    nsamples <- ncol(x)
    ndim <- nsamples - 1

    if (nsamples < 3) {
        stop(paste("Only", nsamples, "columns of data: need at least 3"))
    }

    cn <- colnames(x)
    bad <- rowSums(is.finite(x)) < nsamples

    if (any(bad)) {
        warning("Rows containing infinite values have been removed")
        x <- x[!bad, , drop=FALSE]
    }

    nprobes <- nrow(x)
    top <- min(top, nprobes)

    #
    ##

    gene.selection <- match.arg(gene.selection, c("pairwise", "common"))

    # Distance matrix from pairwise leading fold changes
    dd <- matrix(0, nrow=nsamples, ncol=nsamples, dimnames=list(cn, cn))
    if (gene.selection == "pairwise") {
    # Distance measure is mean of top squared deviations for each pair of arrays
        topindex <- nprobes - top + 1L
        for (i in 2L:(nsamples)) {
            for (j in 1L:(i - 1L)) {
                dists <- (getCols(x, i) - getCols(x, j))^2
                dists <- sort.int(dists, partial = topindex )
                topdist <- dists[topindex:nprobes]
                dd[i, j] <- sqrt(mean(topdist))
            }
        }
    } else {
    # Same genes used for all comparisons
        if (nprobes > top) {
            o <- order(rowMeans( (x-rowMeans(x))^2 ), decreasing=TRUE)
            x <- getRows(x, o[1L:top])
        }
        for (i in 2L:(nsamples)) {
            dists <- (x[, i] - x[, 1:(i-1), drop=FALSE]) ^ 2
            dd[i, 1L:(i-1L)] <- sqrt(colMeans(dists))
        }
    }

    # Multi-dimensional scaling
    a1 <- suppressWarnings(cmdscale(as.dist(dd), k=min(ndim, 8), eig=TRUE))

    # Method for MDS objects
    points <- a1$points

    if (!is.data.frame(groups) && class(groups) != "DataFrame") {
    # Rename for the column name in dataframe
        groups <- data.frame(groups)
    }

    all_col_names <- colnames(groups)
    first_col_name <- all_col_names[1]

    points <- data.frame(points)
    names(points) <- paste0("dim", seq_cols(points))
    points <- data.frame(points, label=labels, groups)

    eigen <- data.frame(
        name = 1:min(ndim, 8),
        eigen = round(a1$eig[1:min(ndim, 8)]/sum(a1$eig), 2)
    )

    plot1 <- glScatter(
        points,
        xval = "dim1",
        yval = "dim2",
        point.size = 4,
        xlab = "Dimension 1",
        ylab = "Dimension 2",
        annot = c("label", all_col_names, "dim1", "dim2"),
        colval = first_col_name,
        main = main,
        info = list(groupsNames=colnames(groups))
    )

    plot2 <- glBar(
        eigen,
        names.arg = "name",
        yval = "eigen",
        main = "Variance Explained",
        xlab = "Dimension",
        ylab = "Proportion",
        height = 300,
        width = 300,
        info = list(dims = ndim)
    )

    link1 <- gllink(2, 1, flag="mds")
    link2 <- gltablink(1, 1, action="highlightById")
    table1 <- glTable(1, c("label", intersect(plot1$anno, colnames(groups))))

    glimma(
        plot1,
        plot2,
        link1,
        table1,
        link2,
        layout = c(1, 2),
        overwrite = TRUE,
        path = path,
        folder = folder,
        html = html,
        launch = launch
    )

    invisible(a1)
}

#' Glimma MDS Plot
#'
#' @template desc_glMDSPlot
#'
#' @author Shian Su, Gordon Smyth
#'
#' @inheritParams glMDSPlot.default
#' @param x the DGEList containing the gene expressions.
#' @param prior.count average count to be added to each observation to avoid taking log of zero. Used only if log=TRUE.
#'
#' @template return_glMDSPlot
#'
#' @method glMDSPlot DGEList
#'
#' @export

glMDSPlot.DGEList <- function (
    x,
    top = 500,
    labels = NULL,
    groups = rep(1, ncol(x)),
    gene.selection = c("pairwise", "common"),
    prior.count = 0.25,
    main = "MDS Plot",
    path = getwd(),
    folder = "glimma-plots",
    html = "MDS-Plot",
    launch = TRUE,
    ...
) {
    labels <- getLabels(x, labels)
    transformed_counts <- edgeR::cpm(x, log=TRUE, prior.count = prior.count)

    glMDSPlot.default(
        transformed_counts,
        top = top,
        labels = labels,
        groups = groups,
        gene.selection = gene.selection,
        main = main,
        path = path,
        folder = folder,
        html = html,
        launch = launch,
        ...
    )
}

#' Glimma MDS Plot
#'
#' @template desc_glMDSPlot
#'
#' @author Shian Su, Gordon Smyth
#'
#' @inheritParams glMDSPlot.default
#' @param x the DESeqDataSet containing the gene expressions.
#' @param prior.count average count to be added to each observation to avoid taking log of zero. Used only if log=TRUE.
#'
#' @template return_glMDSPlot
#'
#' @method glMDSPlot DESeqDataSet
#'
#' @export
glMDSPlot.DESeqDataSet <- function(
    x,
    top = 500,
    labels = NULL,
    groups = NULL,
    gene.selection = c("pairwise", "common"),
    prior.count = 0.25,
    main = "MDS Plot",
    path = getwd(),
    folder = "glimma-plots",
    html = "MDS-Plot",
    launch = TRUE,
    ...
) {
    labels <- getLabels(x, labels)
    transformed_counts <- edgeR::cpm(
        DESeq2::counts(x),
        log = TRUE,
        prior.count = prior.count
    )

    if (is.null(groups)) {
        if (not.null(SummarizedExperiment::colData(x))) {
            groups <- S4Vectors::as.data.frame.DataTable(SummarizedExperiment::colData(x))
        } else {
            groups <- rep(1, ncol(x))
        }
    }



    glMDSPlot.default(
        transformed_counts,
        top = top,
        labels = labels,
        groups = groups,
        gene.selection = gene.selection,
        main = main,
        path = path,
        folder = folder,
        html = html,
        launch = launch,
        ...
    )
}

# extract sample groups based on object class
getLabels <- function(x, labels) {
    
    if (is.null(labels)) {
        if (class(x) == "DGEList") {
            # DGElist get from 
            if (not.null(x$samples$groups)) {
                labels <- rownames(x$samples)
            } else {
                labels <- seq_cols(x)
            }
        } else if (class(x) == "DESeqDataSet") {
            # DESeqDaset
            if (not.null(SummarizedExperiment::colData(x))) {
                labels <- rownames(SummarizedExperiment::colData(x))
            } else {
                labels <- seq_cols(x)
            }
        }
    }

    labels
}
