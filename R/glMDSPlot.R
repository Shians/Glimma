#' Glimma MDS Plot
#'
#' Draw an interactive MDS plot gene expression matrix with distances calculated from most variable genes.
#'
#' @author Shian Su, Gordon Smyth
#'
#' @param x the data.frame containing data to plot.
#' @param ... additional arguments affecting the plots produced. See specific methods for detailed arguments.
#'
#' @return Draws a two-panel interactive MDS plot in an html page. The left panel contains the plot between two MDS dimensions, with annotations displayed on hover. The right panel contains a bar plot of the eigenvalues of each dimension, clicking on any of the bars will plot the corresponding dimension against the next dimension.
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
#' Draw an interactive MDS plot from a gene expression matrix with distances calculated from most variable genes.
#'
#' @author Shian Su, Gordon Smyth
#'
#' @param x the matrix containing the gene expressions.
#' @param top the number of top most variable genes to use.
#' @param labels the labels for each sample.
#' @param groups the experimental group to which samples belong.
#' @param gene.selection "pairwise" if most variable genes are to be chosen for each pair of samples or "common" to select the same genes for all comparisons.
#' @param main the title of the plot.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments.
#'
#' @return Draws a two-panel interactive MDS plot in an html page. The left panel contains the plot between two MDS dimensions, with annotations displayed on hover. The right panel contains a bar plot of the eigenvalues of each dimension, clicking on any of the bars will plot the corresponding dimension against the next dimension.
#'
#' @method glMDSPlot default
#'
#' @importFrom stats cmdscale as.dist
#'
#' @export

# Code taken from plotMDS of limma bioConductor package with alterations
glMDSPlot.default <- function(x, top=500, labels=1:ncol(x),
                            groups=rep(1, ncol(x)), gene.selection="pairwise",
                            main="MDS Plot", path=getwd(),
                            folder="glimma-plots", html="MDS-Plot",
                            launch=TRUE, ...) {
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
        x <- x[!bad, drop=FALSE]
    }

    nprobes <- nrow(x)
    top <- min(top, nprobes)

    #
    ##

    plot.title <- quotify(main)

    gene.selection <- match.arg(gene.selection, c("pairwise", "common"))

    # Distance matrix from pairwise leading fold changes
    dd <- matrix(0, nrow=nsamples, ncol=nsamples, dimnames=list(cn, cn))
    if (gene.selection == "pairwise") {
    # Distance measure is mean of top squared deviations for each pair of arrays
        topindex <- nprobes - top + 1L
        for (i in 2L:(nsamples)) {
            for (j in 1L:(i - 1L)) {
                dist <- sort.int((x[, i] - x[, j])^2, partial=topindex)
                topdist <- dist[topindex:nprobes]
                dd[i, j] <- sqrt(mean(topdist))
            }
        }
    } else {
    # Same genes used for all comparisons
        if (nprobes > top) {
            s <- rowMeans((x-rowMeans(x))^2)
            o <- order(s, decreasing=TRUE)
            x <- x[o[1L:top],, drop=FALSE]
        }
        for (i in 2L:(nsamples))
            dist <- sqrt(colMeans( (x[, i]-x[, 1:(i-1), drop=FALSE])^2 ))
            dd[i, 1L:(i-1L)] <- dist
        axislabel <- "Principal Component"
    }

    # Multi-dimensional scaling
    a1 <- suppressWarnings(cmdscale(as.dist(dd), k=min(ndim, 8), eig=TRUE))

    # Method for MDS objects
    points <- a1$points

    if (!is.data.frame(groups)) {
    # Rename for the column name in dataframe
        group <- groups
        groups <- data.frame(group)
    }

    first.col.name <- colnames(groups)[1]

    points <- data.frame(points)
    names(points) <- paste0("dim", 1:ncol(points))
    points <- data.frame(points, label=labels, groups)

    eigen <- data.frame(name = 1:min(ndim, 8),
                        eigen = round(a1$eig[1:min(ndim, 8)]/sum(a1$eig), 2))

    plot1 <- glScatter(points, xval="dim1", yval="dim2",
                        xlab="Dimension 1", ylab="Dimension 2",
                        annot=c("label", first.col.name, "dim1", "dim2"),
                        colval=first.col.name, main=main,
                        info=list(groupsNames=colnames(groups)))

    plot2 <- glBar(eigen, names.arg="name", yval="eigen",
                    main="Variance Explained",
                    xlab="Dimension", ylab="Proportion",
                    height=300, width=300, info=list(dims=ndim))

    link1 <- gllink(2, 1, flag="mds")

    glimma(plot1, plot2, link1, layout=c(1, 2), overwrite=TRUE,
            path=path, folder=folder, html=html, launch=launch)
}

#' Glimma MDS Plot
#'
#' Draw an interactive MD plot from a DGEList object with distances calculated from most variable genes.
#'
#' @author Shian Su, Gordon Smyth
#'
#' @param x the DGEList containing the gene expressions.
#' @param top the number of top most variable genes to use.
#' @param labels the labels for each sample.
#' @param groups the experimental group to which samples belong.
#' @param gene.selection "pairwise" if most variable genes are to be chosen for each pair of samples or "common" to select the same genes for all comparisons.
#' @param main the title of the plot.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments.
#'
#' @return Draws a two-panel interactive MDS plot in an html page. The left panel contains the plot between two MDS dimensions, with annotations displayed on hover. The right panel contains a bar plot of the eigenvalues of each dimension, clicking on any of the bars will plot the corresponding dimension against the next dimension.
#'
#' @method glMDSPlot DGEList
#'
#' @export
glMDSPlot.DGEList <- function (x, top=500, labels=1:ncol(x),
                            groups=rep(1, ncol(x)), gene.selection="pairwise",
                            main="MDS Plot", path=getwd(),
                            folder="glimma-plots", html="MDS-Plot",
                            launch=TRUE, ...) {
    x <- edgeR::cpm(x, log=TRUE)
    glMDSPlot.default(x, top=500, labels=labels, groups=groups,
                    gene.selection="pairwise", main=main, path=path,
                    folder=folder, html=html, launch=launch, ...)
}
