#' Draw an interactive MD plot
#' 
#' @param x the data.frame containing data to plot.
#' @param ... additional arguments affecting the plots produced.
#' @export
#' @seealso \code{\link{glMDPlot.DGELRT}}, \code{\link{glMDPlot.DGEExact}}, \code{\link{glMDPlot.MArrayLM}}, \code{\link{glMDPlot.DESeqDataSet}}
#' @examples
#' 

glMDPlot <- function(x, ...) {
  UseMethod("glMDPlot")
}

# Hidden internal functions for use by edgeR and limma based plotting

glMDPlot.hidden <- function(plotting.data, sample.exp, display.columns, search.by, default.col, id.column="Symbols", 
							path, folder, html, launch, ...) {

	# Reordering so that significant points appear on top of insignificant points.
	plotting.data <- rbind(plotting.data[plotting.data$col == default.col, ], 
						   plotting.data[plotting.data$col != default.col, ])

	plot1 <- glScatter(plotting.data, xval="logCPM", yval="logFC", xlab="Average log CPM", idval=id.column, ylab="log-fold-change",
					   annot=c(display.columns, "logCPM", "logFC", "Adj.PValue"), flag="mdplot", ndigits=4, ...)

	plot2 <- glScatter(sample.exp, xval="Group", yval=colnames(sample.exp)[3], ylab="logCPM", main=colnames(sample.exp)[3], 
						annot=c("Sample", colnames(sample.exp)[3]), 
						annot.lab=c("Sample", "logCPM"), 
						ndigits=4, hide=TRUE)

	link1 <- link(1, 2, "hover", "yChange", flag="byKey", info="GeneID")
	button1 <- glAutoinput(1, "highlightById", search.by)

	glimma(plot1, plot2, button1, link1, layout=c(1,2), path=path, folder=folder, html=html, overwrite=TRUE, launch=launch)
}

#' Draw an interactive MD plot from a DGELRT object
#' 
#' @param x the DGELRT object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param group the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length as the number of rows of object. If NULL, then all points are plotted in the default color.
#' @param coef integer or character index vector indicating which column of object to plot.
#' @param p.adj.method character vector indicating multiple testing correction method. (defauls to "BH")
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param display.columns character vector containing names of columns to display in mouseover tooltips.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @method glMDPlot DGELRT
#' @export
#' @examples
#' 

glMDPlot.DGELRT <- function(x, counts, anno, groups, samples, status=rep(0, nrow(x)), coef=ncol(x$coefficients),
							p.adj.method="BH", search.by="Symbols", display.columns=c("GeneID"), id.column="GeneID",
							cols=c("#0000FF", "#858585", "#B32222"), path=getwd(), folder="glimma-plots", html="MD-Plot", launch=TRUE, ...) {

	##
	# Input checking
	
	if (length(status) != nrow(x)) {
		stop("The status vector should have same length as the number of columns as main input object.")
	}

	#
	##

	if (any(!is.hex(cols))) {
		cols[!is.hex(cols)] <- CharToHexCol(cols[!is.hex(cols)])
	}

	if (ncol(counts) != length(samples)) {
		stop(paste("columns in count differ from number of samples:", ncol(counts), "vs", length(samples)))
	}

	colourise <- function(x) {
		if (x == -1) {
			return(cols[1])
		} else if (x == 0) {
			return(cols[2])
		} else if (x == 1) {
			return(cols[3])
		}
	}

	col <- sapply(status, colourise)

	plotting.data <- data.frame(anno, x$table, col = col, 
							 Adj.PValue = p.adjust(x$table$PValue, method=p.adj.method))

	rownames(counts) <- make.names(plotting.data[[id.column]])

	sample.exp <- data.frame(Sample = samples,
							 Group = factor(groups),
							 t(cpm(as.matrix(counts), log=TRUE)))

	glMDPlot.hidden(plotting.data, sample.exp, display.columns, search.by, default.col=cols[2], path=path, folder=folder, html=html, launch=launch, ...)
}

#' Draw an interactive MD plot from a DGELRT objet
#' 
#' @param x the DGEExact object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param group the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length as the number of rows of object. If NULL, then all points are plotted in the default color.
#' @param coef integer or character index vector indicating which column of object to plot.
#' @param p.adj.method character vector indicating multiple testing correction method. (defauls to "BH")
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param display.columns character vector containing names of columns to display in mouseover tooltips.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @method glMDPlot DGEExact
#' @export
#' @examples
#' 

glMDPlot.DGEExact <- glMDPlot.DGELRT

#' Draw an interactive MD plot from a MArrayLM object
#' 
#' @param x the MArrayLM object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param group the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length as the number of rows of object. If NULL, then all points are plotted in the default color.
#' @param coef integer or character index vector indicating which column of object to plot.
#' @param p.adj.method character vector indicating multiple testing correction method. (defauls to "BH")
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param display.columns character vector containing names of columns to display in mouseover tooltips.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @method glMDPlot MArrayLM
#' @export
#' @examples
#' 

glMDPlot.MArrayLM <- function(x, counts, anno, groups, samples, status=rep(0, nrow(x)), coef=ncol(x$coefficients),
							p.adj.method="BH", search.by="Symbols", display.columns=c("GeneID"), id.column="GeneID",
							cols=c("#0000FF", "#858585", "#B32222"), path=getwd(), folder="glimma-plots", html="MD-Plot", launch=TRUE, ...) {

	##
	# Input checking
	
	if (length(status) != nrow(x)) {
		stop("The status vector should have same length as the number of columns as main input object.")
	}

	#
	##

	if (any(!is.hex(cols))) {
		cols[!is.hex(cols)] <- CharToHexCol(cols[!is.hex(cols)])
	}

	colourise <- function(x) {
		if (x == -1) {
			return(cols[1])
		} else if (x == 0) {
			return(cols[2])
		} else if (x == 1) {
			return(cols[3])
		}
	}

	col <- sapply(status, colourise)

	plotting.data <- data.frame(logFC = x$coefficients[,coef], 
								 logCPM = x$Amean,
								 col = col,
								 PValue = x$p.value[,coef],
								 Adj.PValue = p.adjust(x$p.value[,coef], method=p.adj.method),
								 anno)

	rownames(counts) <- make.names(plotting.data[[id.column]])

	sample.exp <- data.frame(Sample = samples,
							 Group = factor(groups),
							 t(cpm(as.matrix(counts), log=TRUE)))

	glMDPlot.hidden(plotting.data, sample.exp, display.columns, search.by, default.col=cols[2], path=path, folder=folder, html=html, launch=launch, ...)
}

#' Draw an interactive MD plot from a DESeqDataSet object
#' 
#' @param x the DESeqDataSet object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param group the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length as the number of rows of object. If NULL, then all points are plotted in the default color.
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param display.columns character vector containing names of columns to display in mouseover tooltips.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @method glMDPlot DESeqDataSet
#' @export
#' @examples
#' 

glMDPlot.DESeqDataSet <- function(x, anno, groups, samples, status=rep(0, nrow(x)), 
									search.by="Symbols", display.columns=c("GeneID"), id.column="GeneID",
									cols=c("#0000FF", "#858585", "#B32222"), path=getwd(), folder="glimma-plots", html="MD-Plot", launch=TRUE, ...) {

	##
	# Input checking
	
	if (length(status) != nrow(x)) {
		stop("The status vector should have same length as the number of columns as main input object.")
	}

	#
	##

	if (any(!is.hex(cols))) {
		cols[!is.hex(cols)] <- CharToHexCol(cols[!is.hex(cols)])
	}
	
	res <- results(x)
	res.df <- as.data.frame(res)
	norm.counts <- counts(x)

	colourise <- function(x) {
		if (x == -1) {
			return(cols[1])
		} else if (x == 0) {
			return(cols[2])
		} else if (x == 1) {
			return(cols[3])
		}
	}

	col <- sapply(status, colourise)

	plotting.data <- data.frame(logFC = res.df$log2FoldChange, 
								 logMean = log(res.df$baseMean + 0.5),
								 col = col,
								 PValue = res.df$pvalue,
								 Adj.PValue = res.df$padj,
								 anno)

	rownames(norm.counts) <- make.names(plotting.data[[id.column]])

	sample.exp <- data.frame(Sample = samples,
							 Group = factor(groups),
							 t(cpm(as.matrix(counts(x)), log=TRUE)))
	
	plot1 <- glScatter(plotting.data, xval="logMean", yval="logFC", xlab="Mean Expression", idval="Symbols", ylab="log-fold-change",
					   annot=c(display.columns, "logMean", "logFC", "PValue"), flag="mdplot", ndigits=4, ...)

	plot2 <- glScatter(sample.exp, xval="Group", yval=colnames(sample.exp)[3], ylab="logCPM", main=colnames(sample.exp)[3], 
						annot=c("Sample", colnames(sample.exp)[3]), 
						annot.lab=c("Sample", "logCPM"), 
						ndigits=4, hide=TRUE)

	link1 <- link(1, 2, "hover", "yChange", flag="byKey", info="GeneID")
	button1 <- glAutoinput(1, "highlightById", search.by)

	glimma(plot1, plot2, button1, link1, layout=c(1,2), path=path, folder=folder, html=html, overwrite=TRUE, launch=launch)
}