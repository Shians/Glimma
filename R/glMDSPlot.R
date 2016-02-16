#' Draw an interactive MDS plot
#' 
#' @param x the data.frame containing data to plot.
#' @param ... additional arguments affecting the plots produced.
#' @return writes an interactive MDS plot
#' @export
#' @examples
#' 

glMDSPlot <- function(x, ...) {
  UseMethod("glMDSPlot")
}

#' Draw an interactive MD plot from a DGEList object
#' @export

glMDSPlot.DGEList <- function (x, top=500, labels=1:ncol(x), gene.selection="pairwise",
								main="MDS Plot") {
	x <- cpm(x, log=TRUE)
	glMDSPlot.default(x, top=500, labels=labels, gene.selection="pairwise", main=main)
}

#' @export

# Code taken from plotMDS of limma bioConductor package with alterations
glMDSPlot.hidden <- function(x, top=500, labels=NULL, gene.selection="pairwise",
								main=NULL) {
	#	Multi-dimensional scaling with top-distance
	#	Di Wu and Gordon Smyth
	#	19 March 2009.  Last modified 14 Jan 2015
	#	Modified by Shian Su on 25 Jan 2016
	##
	# Check Inputs
	x <- as.matrix(x)
	nsamples <- ncol(x)

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
	dd <- matrix(0, nrow=nsamples, ncol=nsamples, dimnames=list(cn,cn))
	if (gene.selection == "pairwise") {
	# Distance measure is mean of top squared deviations for each pair of arrays
		topindex <- nprobes - top + 1L
		for (i in 2L:(nsamples)) {
			for (j in 1L:(i - 1L)) {
				dd[i,j] <- sqrt(mean(sort.int((x[, i] - x[, j]) ^ 2, partial=topindex)[topindex:nprobes]))
			}
		}
	} else {
	# Same genes used for all comparisons
		if (nprobes > top) {
			s <- rowMeans((x-rowMeans(x))^2)
			o <- order(s,decreasing=TRUE)
			x <- x[o[1L:top],,drop=FALSE]
		}
		for (i in 2L:(nsamples))
			dd[i,1L:(i-1L)] <- sqrt(colMeans((x[,i]-x[,1:(i-1),drop=FALSE])^2))
		axislabel <- "Principal Component"
	}

	# Multi-dimensional scaling
	a1 <- suppressWarnings(cmdscale(as.dist(dd), k=min(nsamples, 8), eig=TRUE))
	class(a1) <- "MDS"

	glMDSPlot.hidden(a1, labels=labels, main=main)
}

glMDSPlot.hidden <- function(x, labels=NULL, main=NULL) {
	#	Method for MDS objects
	points <- x$points
	
	points <- data.frame(points)
	names(points) <- paste0("dim", 1:ncol(points))
	points <- data.frame(points, label=labels)

	eigen <- data.frame(name = 1:8, eigen=round(x$eig[1:8]/sum(x$eig), 2))

	plot1 <- glScatter(points, xval="dim1", yval="dim2", xlab="Dimension 1", ylab="Dimension 2",
						 colval="label", main=main)
	plot2 <- glBar(eigen, names.arg="name", yval="eigen", ylab="Magnitude", height=300, width=300)
	link1 <- link(2, 1, flag="mds")
	# TODO: Add labels to annotation
	glimma(plot1, plot2, link1, layout=c(1, 2), annot=c("label", "dim1", "dim2"),
			html="MDS", overwrite=TRUE)
}
