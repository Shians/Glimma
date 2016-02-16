#' Draw an interactive MD plot
#' 
#' @param x the data.frame containing data to plot.
#' @param ... additional arguments affecting the plots produced.
#' @export
#' @seealso \code{\link{glMDPlot.DGELRT}}, \code{\link{glMDPlot.MArrayLM}}
#' @examples
#' 

glMDPlot <- function(x, ...) {
  UseMethod("glMDPlot")
}

#' Draw an interactive MD plot
#' 
#' @param x the DGELRT object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param group the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @return writes an interactive MD plot
#' @export
#' @examples
#' 

glMDPlot.DGELRT <- function(x, counts, anno, groups, samples, status=rep(0, nrow(x)), coef=ncol(x$coefficients),
							p.adj.method="BH", search.by="Symbols", display.cols=c("GeneID"), ...) {

	if (ncol(counts) != length(samples)) {
		stop(paste("columns in count differ from number of samples:", ncol(counts), "vs", length(samples)))
	}

	colourise <- function(x) {
		if (x == 0) {
			return("#858585")
		} else if (x == 1) {
			return("#B32222")
		} else if (x == -1) {
			return("#0000FF")
		}
	}

	col <- sapply(status, colourise)

	data.table <- data.frame(anno, x$table, col = col, 
							 Adj.PValue = p.adjust(x$table$PValue, method=p.adj.method))

	sample.exp <- data.frame(Sample = samples,
							 Group = factor(groups),
							 t(cpm(as.matrix(counts), log=TRUE)))

	xval <- "logCPM"
	yval <- "logFC"
	idval <- "Symbols"

	plot1 <- glScatter(data.table, xval=xval, yval=yval, xlab="Average log CPM", idval=search.by, ylab="log-fold-change",
					   annot=c(display.cols, "logCPM", "logFC", "Adj.PValue"), flag="mdplot", ndigits=4)

	plot2 <- glScatter(sample.exp, xval="Group", yval=colnames(sample.exp)[3], ylab="logCPM", main=colnames(sample.exp)[3], 
						annot=c("Sample", colnames(sample.exp)[3]), 
						annot.lab=c("Sample", "logCPM"), 
						ndigits=4, hide=TRUE)
	
	link1 <- link(1, 2, "hover", "yChange", flag="byKey", info="GeneID")
	button1 <- glAutoinput(1, "highlightById", "Symbol")

	glimma(plot1, plot2, button1, link1, layout=c(1,2), overwrite=TRUE, ...)
}

#' Draw an interactive MD plot
#' 
#' @param x the DGEExact object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param group the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @return writes an interactive MD plot
#' @export
#' @examples
#' 

glMDPlot.DGEExact <- glMDPlot.DGELRT

#' Draw an interactive MD plot
#' 
#' @param x the MArrayLM object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param group the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @return writes an interactive MD plot
#' @export
#' @examples
#' 

glMDPlot.MArrayLM <- function(x, counts, anno, groups, samples, status=rep(0, nrow(x)), coef=ncol(x$coefficients),
							p.adj.method="BH", search.by="Symbol", display.cols=c("GeneID"), ...) {

	colourise <- function(x) {
		if (x == 0) {
			return("#858585")
		} else if (x == 1) {
			return("#B32222")
		} else if (x == -1) {
			return("#0000FF")
		}
	}

	col <- sapply(status, colourise)

	data.table <- data.frame(logFC = x$coefficients[,coef], 
							 logCPM = x$Amean,
							 col = col,
							 PValue = x$p.value[,coef], anno)

	sample.exp <- data.frame(Sample = samples,
							 Group = factor(groups),
							 t(cpm(as.matrix(counts), log=TRUE)))


	reload(inst("Glimma"))
	plot1 <- glScatter(plotting.data, xval="logCPM", yval="logFC", xlab="Average log CPM", idval="Symbols", ylab="log-fold-change",
					   annot=c("Symbols", "logCPM", "logFC"), flag="mdplot")

	plot2 <- glScatter(sample.exp, xval="Group", yval=colnames(sample.exp)[3], ylab="logCPM", main=colnames(sample.exp)[3], 
						annot=c("Sample", colnames(sample.exp)[3]), 
						annot.lab=c("Sample", "logCPM"), 
						ndigits=4, hide=TRUE)

	link1 <- link(1, 2, "hover", "yChange", flag="byKey", info="GeneID")
	button1 <- glAutoinput(1, "highlightById", "Symbol")

	glimma(plot1, plot2, button1, link1, layout=c(1,2), overwrite=TRUE)
}

#' Draw an interactive MD plot
#' 
#' @param x the DGEExact object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param group the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @return writes an interactive MD plot
#' @export
#' @examples
#' 

glMDSPlot.DESeqResults <- function(x, anno) {
	
	res <- results(x)
	res.df <- as.data.frame(res)
	norm.counts <- counts(x, normalized=TRUE)

	
}