#' Glimma MD Plot
#'
#' Draw an interactive MD plot
#'
#' @author Shian Su
#' 
#' @param x the DE object to plot.
#' @param ... additional arguments affecting the plots produced. See specific methods for detailed arguments.
#'
#' @seealso \code{\link{glMDPlot.default}}, \code{\link{glMDPlot.DGELRT}}, \code{\link{glMDPlot.DGEExact}}, \code{\link{glMDPlot.MArrayLM}}, \code{\link{glMDPlot.DESeqDataSet}}
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot
#' shows the log-fold-change vs average expression. The right plot shows the
#' expression levels of a particular gene of each sample. Hovering over points
#' on left plot will plot expression level for corresponding gene, clicking
#' on points will fix the expression plot to gene. Clicking on rows on the table
#' has the same effect as clicking on the corresponding gene in the plot.
#'
#' @examples
#' library(limma)
#' library(edgeR)
#'
#' data(lymphomaRNAseq)
#' x <- lymphomaRNAseq
#'
#' sel <- rowSums(cpm(x$counts)>0.5)>=3
#' x <- x[sel,]
#'
#' genotype <- relevel(x$samples$group, "Smchd1-null")
#' x <- calcNormFactors(x, method="TMM")
#' des <- model.matrix(~genotype)
#'
#' ## Apply voom with sample quality weights and fit linear model
#' v <- voomWithQualityWeights(x, design=des, normalization="none", plot=FALSE)
#' vfit <- lmFit(v,des)
#'
#' ## Apply treat relative to a fold-change of 1.5
#' vtfit <- treat(vfit,lfc=log2(1.5))
#' vfit <- eBayes(vfit)
#' results <- decideTests(vfit,p.value=0.01)
#'
#' \donttest{
#' glMDPlot(vfit, counts=x$counts, anno=x$genes, groups=genotype, samples=1:7,
#'          status=results[,2], main="MD plot: Wild-type vs Smchd1",
#'          display.columns=c("Symbols", "GeneID", "GeneName"),
#'          folder="Smchd1-Lymphoma")
#' }
#'
#' @export

glMDPlot <- function(x, ...) {
    UseMethod("glMDPlot")
}

#' Glimma MD Plot
#'
#' Draw an interactive MD plot from a data.frame
#'
#' @author Shian Su
#'
#' @param x the data.frame object containing expression and fold change values.
#' @param xval the column to plot on x axis of left plot. 
#' @param yval the column to plot on y axis of left plot.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param groups the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length as the number of rows of object. If NULL, then all points are plotted in the default colour.
#' @param transform TRUE if counts are raw and should be cpm transformed, FALSE if counts are already transformed to expression scale.
#' @param side.xlab label for x axis on right side plot.
#' @param side.ylab label for y axis on right side plot.
#' @param side.log TRUE to plot expression on the side plot on log scale.
#' @param side.gridstep intervals along which to place grid lines on y axis. Currently only available for linear scale.
#' @param xlab the label on the x axis for the left plot.
#' @param ylab the label on the y axis for the left plot.
#' @param search.by the name of the column which will be used to search for data points if table is not used. (should contain unique values)
#' @param jitter the amount of jitter to apply to the samples in the expressions plot.
#' @param id.column the column containing unique identifiers for each gene.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips and table.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point on the expression plot.
#' @param table logical variable for whether a table of the data should appear on the bottom of the HTML page.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot. (main, xlab, ylab can be set for the left plot)
#' 
#' @return Draws a two-panel interactive MD plot in an html page. The left plot
#' shows the log-fold-change vs average expression. The right plot shows the
#' expression levels of a particular gene of each sample. Hovering over points
#' on left plot will plot expression level for corresponding gene, clicking
#' on points will fix the expression plot to gene. Clicking on rows on the table
#' has the same effect as clicking on the corresponding gene in the plot.
#'
#' @method glMDPlot default
#'
#' @importFrom stats p.adjust
#' @importFrom methods is
#' @importFrom edgeR cpm
#'
#' @export

glMDPlot.default <- function(x, xval, yval, counts=NULL, anno=NULL,
                        groups, samples=NULL,
                        status=rep(0, nrow(x)), transform=TRUE,
                        side.xlab="Group", side.ylab="logCPM",
                        side.log=FALSE,
                        side.gridstep=ifelse(!transform || side.log, FALSE, 0.5),
                        xlab=xval, ylab=yval,
                        search.by="Symbols", jitter=30,
                        id.column="GeneID", display.columns=id.column,
                        cols=c("#0000FF", "#858585", "#B32222"),
                        sample.cols=rep("#1f77b4", ncol(counts)),
                        table=TRUE,
                        path=getwd(), folder="glimma-plots", html="MD-Plot",
                        launch=TRUE, ...) {

    ##
    # Input checking

    checkThat(length(status), sameAs(nrow(x)))

    if (id.column %in% colnames(x)) {
        checkThat(x[[id.column]], isUnique)
    } else if (id.column %in% colnames(anno)) {
        checkThat(anno[[id.column]], isUnique)
    } else {
        stop(paste("column", quotify(id.column), "cannot be found in x or anno."))
    }

    #
    ##

    # Input checking


    cols <- convertColsToHex(cols)

    if (!is.null(counts)) {
        if (!is.null(samples)) {
            checkThat(ncol(counts), sameAs(length(samples)))
        }

        if (side.log && any(counts == 0)) {
            stop("There are zeroes in expression matrix which cannot be plotted on log-scale, consider adding small offset.")
        }
    }

    #
    ##

    jitter <- ifelse(is.numeric(groups), 0, jitter)

    col <- convertStatusToCols(status, cols)

    if (is.null(anno)) {
        plotting.data <- data.frame(x, col = col)
    } else {
        plotting.data <- data.frame(anno, x, col = col)
    }

    if (!is.null(counts)) {
        rownames(counts) <- make.names(plotting.data[[id.column]])

        if (transform) {
            tr.counts <- t(as.matrix(edgeR::cpm(counts, log=TRUE)))
        } else {
            tr.counts <- t(as.matrix(counts))
        }
    }

    if (!is.null(counts)) {
        if (is(groups, "numeric")) {
            sample.exp <- data.frame(Sample = samples,
                                 col = as.hexcol(sample.cols),
                                 Group = groups,
                                 tr.counts)
        } else {
            sample.exp <- data.frame(Sample = samples,
                                 col = as.hexcol(sample.cols),
                                 Group = factor(groups),
                                 tr.counts)
        }
    } else {
        sample.exp <- NULL
    }

    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    # Reordering so that significant points appear on top of insignificant
    # points.

    plotting.data <- rbind(plotting.data[plotting.data$col == cols[2], ],
                           plotting.data[plotting.data$col != cols[2], ])

    plot1 <- glScatter(plotting.data, xval=xval, yval=yval,
                    xlab=xlab, idval=id.column, ylab=ylab,
                    annot=c(display.columns, xval, yval), flag="mdplot",
                    ndigits=4, info=list(search.by=search.by), ...)

    if (!is.null(counts)) {

        link1 <- gllink(1, 2, "hover", "yChange", flag="byKey", info=id.column)
        link2 <- gllink(1, 2, "click", "yChange", flag="byKey", info=id.column)

        if (side.gridstep) {
            plot2 <- glScatter(sample.exp, xval="Group",
                                yval=colnames(sample.exp)[4],
                                xlab=side.xlab, ylab=side.ylab,
                                main=colnames(sample.exp)[4],
                                colval="col",
                                annot=c("Sample", colnames(sample.exp)[4]),
                                annot.lab=c("Sample", "logCPM"),
                                x.jitter = jitter,
                                ndigits=4, hide=TRUE,
                                ystep=side.gridstep, ygrid=TRUE)
        } else {
            plot2 <- glScatter(sample.exp, xval="Group",
                                yval=colnames(sample.exp)[4],
                                xlab=side.xlab, ylab=side.ylab,
                                main=colnames(sample.exp)[4],
                                colval="col",
                                annot=c("Sample", colnames(sample.exp)[4]),
                                annot.lab=c("Sample", "logCPM"),
                                x.jitter = jitter,
                                ndigits=4, hide=TRUE, ygrid=FALSE)
        }
    } else {
        link1 <- NULL
        link2 <- NULL
        plot2 <- NULL
    }

    draw.plots(table, display.columns, search.by, xval, yval,
                plot1, plot2, link1, link2, path, folder, html,
                launch)
}

#' Glimma MD Plot
#'
#' Draw an interactive MD plot from a DGELRT object
#'
#' @author Shian Su
#' 
#' @param x the DGELRT object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param groups the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length as the number of rows of object. If NULL, then all points are plotted in the default colour.
#' @param transform TRUE if counts are raw and should be cpm transformed, FALSE if counts are already transformed to expression scale.
#' @param side.xlab label for x axis on right side plot.
#' @param side.ylab label for y axis on right side plot.
#' @param side.log TRUE to plot expression on the side plot on log scale.
#' @param side.gridstep intervals along which to place grid lines on y axis. Currently only available for linear scale.
#' @param p.adj.method character vector indicating multiple testing correction method. See \code{\link{p.adjust}} for available methods. (defaults to "BH")
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param jitter the amount of jitter to apply to the samples in the expressions plot.
#' @param id.column the column containing unique identifiers for each gene.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips and table.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point on the expression plot.
#' @param table logical variable for whether a table of the data should appear on the bottom of the HTML page.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot. (main, xlab, ylab can be set for the left plot)
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot
#' shows the log-fold-change vs average expression. The right plot shows the
#' expression levels of a particular gene of each sample. Hovering over points
#' on left plot will plot expression level for corresponding gene, clicking
#' on points will fix the expression plot to gene. Clicking on rows on the table
#' has the same effect as clicking on the corresponding gene in the plot.
#' 
#' @method glMDPlot DGELRT
#'
#' @importFrom stats p.adjust
#' @importFrom methods is
#'
#' @export

glMDPlot.DGELRT <- function(x, counts=NULL, anno=NULL,
                            groups=rep(0, ncol(x)), samples=NULL,
                            status=rep(0, nrow(x)), transform=TRUE,
                            side.xlab="Group", side.ylab="logCPM",
                            side.log=FALSE,
                            side.gridstep=ifelse(!transform || side.log, FALSE, 0.5),
                            p.adj.method="BH", search.by="Symbols", jitter=30,
                            id.column="GeneID", display.columns=NULL,
                            cols=c("#0000FF", "#858585", "#B32222"),
                            sample.cols=rep("#1f77b4", ncol(counts)),
                            table=TRUE,
                            path=getwd(), folder="glimma-plots", html="MD-Plot",
                            launch=TRUE, ...) {

    ##
    # Input checking

    checkThat(length(status), sameAs(nrow(x)))

    if (!is.null(counts)) {
        if (!is.null(samples)) {
            checkThat(ncol(counts), sameAs(length(samples)))
        }

        if (side.log && any(counts == 0)) {
            stop("There are zeroes in expression matrix which cannot be plotted on log-scale, consider adding small offset.")
        }
    }

    ## Not the correct check, need to check in x and anno, commented out until fixed.
    # if (anyDuplicated(x[[id.column]])) {
    #     stop(paste("column", quotify(id.column), "in x contains duplicated values."))
    # }

    #
    ##

    ##
    # Value initialisation

    xval <- "logCPM"
    yval <- "logFC"
    anno <- makeAnno(x, anno)
    cols <- convertColsToHex(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    if (is.null(samples)) {
        if (!is.null(counts)) {
            if (!is.null(colnames(counts))) {
                samples <- colnames(counts)
            } else {
                samples <- 1:ncol(counts)
            }
        }
    }

    jitter <- ifelse(is.numeric(groups), 0, jitter)

    #
    ##

    col <- convertStatusToCols(status, cols)

    if (is.null(anno)) {
        plotting.data <- data.frame(x$table, col = col,
                                    Adj.PValue = stats::p.adjust(x$table$PValue,
                                    method = p.adj.method))
    } else {
        plotting.data <- data.frame(anno, x$table, col = col,
                                    Adj.PValue = stats::p.adjust(x$table$PValue,
                                    method = p.adj.method))
    }

    if (!is.null(counts)) {
        rownames(counts) <- make.names(plotting.data[[id.column]])

        if (transform) {
            tr.counts <- t(edgeR::cpm(as.matrix(counts), log=TRUE))
        } else {
            tr.counts <- t(as.matrix(counts))
        }

        if (is(groups, "numeric")) {
            sample.exp <- data.frame(Sample = samples,
                                     col = as.hexcol(sample.cols),
                                     Group = groups,
                                     tr.counts)
        } else {
            sample.exp <- data.frame(Sample = samples,
                                     col = as.hexcol(sample.cols),
                                     Group = factor(groups),
                                     tr.counts)
        }
    } else {
        sample.exp <- NULL
    }

    plotWithTable(plotting.data, sample.exp, display.columns, search.by,
                id.column=id.column, default.col=cols[2], jitter=jitter,
                path=path, folder=folder, html=html, launch=launch,
                table=table, xval=xval, yval=yval,
                xlab="Average log CPM", ylab="log-fold-change",
                side.xlab=side.xlab, side.ylab=side.ylab, side.log=side.log,
                side.gridstep=side.gridstep,
                ...)
}

#' Glimma MD Plot
#'
#' Draw an interactive MD plot from a DGELRT objet
#'
#' @author Shian Su
#'
#' @param x the DGEExact object.
#' @inheritParams glMDPlot.DGELRT
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot
#' shows the log-fold-change vs average expression. The right plot shows the
#' expression levels of a particular gene of each sample. Hovering over points
#' on left plot will plot expression level for corresponding gene, clicking
#' on points will fix the expression plot to gene. Clicking on rows on the table
#' has the same effect as clicking on the corresponding gene in the plot.
#'
#' @method glMDPlot DGEExact
#'
#' @importFrom stats p.adjust
#' @importFrom methods is
#'
#' @export

glMDPlot.DGEExact <- glMDPlot.DGELRT

#' Glimma MD Plot
#'
#' Draw an interactive MD plot from a MArrayLM object
#'
#' @author Shian Su
#' 
#' @param x the MArrayLM object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param groups the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length as the number of rows of object. If NULL, then all points are plotted in the default colour.
#' @param transform TRUE if counts are raw and should be cpm transformed, FALSE if counts are already transformed to expression scale.
#' @param side.xlab label for x axis on right side plot.
#' @param side.ylab label for y axis on right side plot.
#' @param side.log TRUE to plot expression on the side plot on log scale.
#' @param side.gridstep intervals along which to place grid lines on y axis. Currently only available for linear scale.
#' @param coef integer or character index vector indicating which column of object to plot.
#' @param p.adj.method character vector indicating multiple testing correction method. See \code{\link{p.adjust}} for available methods. (defaults to "BH")
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param jitter the amount of jitter to apply to the samples in the expressions plot.
#' @param id.column the column containing unique identifiers for each gene.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips and table.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point on the expression plot.
#' @param table logical variable for whether a table of the data should appear on the bottom of the HTML page.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot. (main, xlab, ylab can be set for the left plot)
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot
#' shows the log-fold-change vs average expression. The right plot shows the
#' expression levels of a particular gene of each sample. Hovering over points
#' on left plot will plot expression level for corresponding gene, clicking
#' on points will fix the expression plot to gene. Clicking on rows on the table
#' has the same effect as clicking on the corresponding gene in the plot.
#'
#' @examples
#' \donttest{
#' library(limma)
#' library(edgeR)
#'
#' data(lymphomaRNAseq)
#' x <- lymphomaRNAseq
#'
#' sel <- rowSums(cpm(x$counts)>0.5)>=3
#' x <- x[sel,]
#'
#' genotype <- relevel(x$samples$group, "Smchd1-null")
#' x <- calcNormFactors(x, method="TMM")
#' des <- model.matrix(~genotype)
#'
#' ## Apply voom with sample quality weights and fit linear model
#' v <- voomWithQualityWeights(x, design=des, normalization="none", plot=FALSE)
#' vfit <- lmFit(v,des)
#'
#' ## Apply treat relative to a fold-change of 1.5
#' vtfit <- treat(vfit,lfc=log2(1.5))
#' vfit <- eBayes(vfit)
#' results <- decideTests(vfit,p.value=0.01)
#'
#' glMDPlot(vfit, counts=x$counts, anno=x$genes, groups=genotype, samples=1:7,
#'          status=results[,2], main="MD plot: Wild-type vs Smchd1",
#'          display.columns=c("Symbols", "GeneID", "GeneName"),
#'          folder="Smchd1-Lymphoma")
#' }
#'
#' @method glMDPlot MArrayLM
#'
#' @importFrom stats p.adjust
#' @importFrom methods is
#'
#' @export

glMDPlot.MArrayLM <- function(x, counts=NULL, anno=NULL,
                            groups=rep(0, ncol(x)), samples=NULL,
                            status=rep(0, nrow(x)), transform=TRUE,
                            side.xlab="Group", side.ylab="logCPM",
                            side.log=FALSE,
                            side.gridstep=ifelse(!transform || side.log, FALSE, 0.5),
                            coef=ncol(x$coefficients),
                            p.adj.method="BH", search.by="Symbols", jitter=30,
                            id.column="GeneID", display.columns=NULL,
                            cols=c("#0000FF", "#858585", "#B32222"),
                            sample.cols=rep("#1f77b4", ncol(counts)),
                            table=TRUE,
                            path=getwd(), folder="glimma-plots", html="MD-Plot",
                            launch=TRUE, ...) {

    ##
    # Input checking

    if (is(status, "numeric")) {
        checkThat(length(status), sameAs(nrow(x)))
    } else if (is(status, "matrix")) {
        checkThat(nrow(status), sameAs(nrow(x)))
    }

    if (!is.null(counts)) {
        if (!is.null(samples)) {
            if (ncol(counts) != length(samples)) {
                stop(paste("columns in count differ from number of samples:", ncol(counts), "vs", length(samples)))
            }    
        }
        
        if (side.log && any(counts == 0)) {
            stop("There are zeroes in expression matrix which cannot be plotted on log-scale, consider adding small offset.")
        }
    }

    ## Not the correct check, need to check in x and anno, commented out until fixed.
    # if (anyDuplicated(x[[id.column]])) {
    #     stop(paste("column", quotify(id.column), "in x contains duplicated values."))
    # }

    #
    ##

    ##
    # Value initialisation

    xval <- "logCPM"
    yval <- "logFC"
    anno <- makeAnno(x, anno)
    cols <- convertColsToHex(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    if (!is.null(ncol(status))) {
        if (ncol(status) > 1) {
            status <- status[, coef]
        }
    }

    if (is.null(samples)) {
        if (!is.null(counts)) {
            if (!is.null(colnames(counts))) {
                samples <- colnames(counts)
            } else {
                samples <- 1:ncol(counts)
            }
        }
    }

    jitter <- ifelse(is.numeric(groups), 0, jitter)

    #
    ##

    col <- convertStatusToCols(status, cols)

    Adj.PValue <- stats::p.adjust(x$p.value[, coef], method=p.adj.method)
    if (is.null(anno)) {
        plotting.data <- data.frame(logFC = x$coefficients[, coef],
                                     logCPM = x$Amean,
                                     col = col,
                                     PValue = x$p.value[, coef],
                                     Adj.PValue = Adj.PValue)
    } else {
        plotting.data <- data.frame(logFC = x$coefficients[, coef],
                                     logCPM = x$Amean,
                                     col = col,
                                     PValue = x$p.value[, coef],
                                     Adj.PValue = Adj.PValue,
                                     anno)
    }

    if (!is.null(counts)) {
        rownames(counts) <- make.names(plotting.data[[id.column]])

        if (transform) {
            tr.counts <- t(as.matrix(edgeR::cpm(counts, log=TRUE)))
        } else {
            tr.counts <- t(as.matrix(counts))
        }

        if (is(groups, "numeric")) {
            sample.exp <- data.frame(Sample = samples,
                                     col = as.hexcol(sample.cols),
                                     Group = groups,
                                     tr.counts)
        } else {
            sample.exp <- data.frame(Sample = samples,
                                     col = as.hexcol(sample.cols),
                                     Group = factor(groups),
                                     tr.counts)
        }
    } else {
        sample.exp <- NULL
    }

    plotWithTable(plotting.data, sample.exp, display.columns, search.by,
                id.column=id.column, default.col=cols[2], jitter=jitter,
                path=path, folder=folder, html=html, launch=launch,
                table=table, xval=xval, yval=yval,
                xlab="Average log CPM", ylab="log-fold-change",
                side.xlab=side.xlab, side.ylab=side.ylab, side.log=side.log,
                side.gridstep=side.gridstep,
                ...)
}

#' Glimma MD Plot
#'
#' Draw an interactive MD plot from a DESeqDataSet object
#'
#' @author Shian Su
#'
#' @param x the DESeqDataSet object.
#' @param anno the data.frame containing gene annotations.
#' @param groups the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length as the number of rows of object. If NULL, then all points are plotted in the default colour.
#' @param transform TRUE if counts are raw and should be cpm transformed, FALSE if counts are already transformed to expression scale.
#' @param side.xlab label for x axis on right side plot.
#' @param side.ylab label for y axis on right side plot.
#' @param side.log TRUE to plot expression on the side plot on log scale.
#' @param side.gridstep intervals along which to place grid lines on y axis. Currently only available for linear scale.
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param jitter the amount of jitter to apply to the samples in the expressions plot.
#' @param id.column the column containing unique identifiers for each gene.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips and table.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point on the expression plot.
#' @param table logical variable for whether a table of the data should appear on the bottom of the HTML page.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot. (main, xlab, ylab can be set for the left plot)
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot
#' shows the log-fold-change vs average expression. The right plot shows the
#' expression levels of a particular gene of each sample. Hovering over points
#' on left plot will plot expression level for corresponding gene, clicking
#' on points will fix the expression plot to gene. Clicking on rows on the table
#' has the same effect as clicking on the corresponding gene in the plot.
#'
#' @method glMDPlot DESeqDataSet
#'
#' @importFrom methods is
#' @importFrom edgeR cpm
#' @importFrom DESeq2 counts results DESeqDataSet
#'
#' @export

glMDPlot.DESeqDataSet <- function(x, anno, groups, samples,
                                status=rep(0, nrow(x)), transform=TRUE,
                                side.xlab="Group", side.ylab="logMean",
                                side.log=FALSE,
                                side.gridstep=ifelse(!transform || side.log, FALSE, 0.5),
                                search.by="Symbols",
                                jitter=30, id.column="GeneID",
                                display.columns=NULL,
                                cols=c("#0000FF", "#858585", "#B32222"),
                                sample.cols=rep("#1f77b4", ncol(x)),
                                table=TRUE,
                                path=getwd(), folder="glimma-plots",
                                html="MD-Plot", launch=TRUE, ...) {

    ##
    # Input checking

    checkThat(length(status), sameAs(nrow(x)))

    if (!is.null(counts)) {
        if (side.log && any(counts == 0)) {
            stop("There are zeroes in expression matrix which cannot be plotted on log-scale, consider adding small offset.")
        }
    }

    ## Not the correct check, need to check in x and anno, commented out until fixed.
    # if (anyDuplicated(x[[id.column]])) {
    #     stop(paste("column", quotify(id.column), "in x contains duplicated values."))
    # }

    #
    ##

    ##
    # Value initialisation

    xval <- "logMean"
    yval <- "logFC"

    cols <- convertColsToHex(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    #
    ##

    res <- DESeq2::results(x)
    res.df <- as.data.frame(res)
    gene.counts <- DESeq2::counts(x)

    col <- convertStatusToCols(status, cols)

    plotting.data <- data.frame(logFC = res.df$log2FoldChange,
                                 logMean = log(res.df$baseMean + 0.5),
                                 col = col,
                                 PValue = res.df$pvalue,
                                 Adj.PValue = res.df$padj,
                                 anno)

    # Reordering so that significant points appear on top of insignificant
    # points.
    plotting.data <- rbind(plotting.data[plotting.data$col == cols[2], ],
                           plotting.data[plotting.data$col != cols[2], ])

    rownames(gene.counts) <- make.names(plotting.data[[id.column]])

    if (transform) {
        tr.counts <- t(as.matrix(edgeR::cpm(gene.counts, log=TRUE)))
    } else {
        tr.counts <- t(as.matrix(gene.counts))
    }

    sample.exp <- data.frame(Sample = samples,
                             col = as.hexcol(sample.cols),
                             Group = factor(groups),
                             tr.counts)

    plotWithTable(plotting.data, sample.exp, display.columns, search.by,
                    id.column=id.column, default.col=cols[2], jitter=jitter,
                    path=path, folder=folder, html=html, launch=launch,
                    table=table, xval=xval, yval=yval,
                    xlab="Mean Expression", ylab="log-fold-change",
                    side.xlab=side.xlab, side.ylab=side.ylab, side.log=side.log,
                    side.gridstep=side.gridstep,
                    ...)
}

#' Glimma MD Plot
#'
#' Draw an interactive MD plot from a DESeqResults object
#'
#' @author Shian Su
#' 
#' @inheritParams glMDPlot.DESeqDataSet
#' @param x the DESeqResults object.
#' @param counts the matrix containing all counts.
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot
#' shows the log-fold-change vs average expression. The right plot shows the
#' expression levels of a particular gene of each sample. Hovering over points
#' on left plot will plot expression level for corresponding gene, clicking
#' on points will fix the expression plot to gene. Clicking on rows on the table
#' has the same effect as clicking on the corresponding gene in the plot.
#'
#' @method glMDPlot DESeqResults
#'
#' @importFrom methods is
#' @importFrom edgeR cpm
#' @importFrom DESeq2 counts results DESeqResults
#'
#' @export

glMDPlot.DESeqResults <- function(x, counts, anno, groups, samples,
                                status=rep(0, nrow(x)), transform=TRUE,
                                side.xlab="Group", side.ylab="logCPM",
                                side.log=FALSE,
                                side.gridstep=ifelse(!transform || side.log, FALSE, 0.5),
                                search.by="Symbols",
                                jitter=30, id.column="GeneID",
                                display.columns=NULL,
                                cols=c("#0000FF", "#858585", "#B32222"),
                                sample.cols=rep("#1f77b4", ncol(counts)),
                                table=TRUE,
                                path=getwd(), folder="glimma-plots",
                                html="MD-Plot", launch=TRUE, ...) {

    ##
    # Input checking

    checkThat(length(status), sameAs(nrow(x)))

    if (!is.null(counts)) {
        if (!is.null(samples)) {
            checkThat(ncol(counts), sameAs(length(samples)))
        }    
        

        if (side.log && any(counts == 0)) {
            stop("There are zeroes in expression matrix which cannot be plotted on log-scale, consider adding small offset.")
        }
    }

    ## Not the correct check, need to check in x and anno, commented out until fixed.
    # if (anyDuplicated(x[[id.column]])) {
    #     stop(paste("column", quotify(id.column), "in x contains duplicated values."))
    # }

    #
    ##

    ##
    # Value initialisation

    xval <- "logMean"
    yval <- "logFC"

    cols <- convertColsToHex(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    #
    ##

    res <- x
    res.df <- as.data.frame(res)
    gene.counts <- counts

    col <- convertStatusToCols(status, cols)

    plotting.data <- data.frame(logFC = res.df$log2FoldChange,
                                 logMean = log(res.df$baseMean + 0.5),
                                 col = col,
                                 PValue = res.df$pvalue,
                                 Adj.PValue = res.df$padj,
                                 anno)

    # Reordering so that significant points appear on top of insignificant points.
    plotting.data <- rbind(plotting.data[plotting.data$col == cols[2], ],
                           plotting.data[plotting.data$col != cols[2], ])

    rownames(gene.counts) <- make.names(plotting.data[[id.column]])

    if (!is.null(counts)) {
        if (transform) {
            tr.counts <- t(as.matrix(edgeR::cpm(gene.counts, log=TRUE)))
        } else {
            tr.counts <- t(as.matrix(gene.counts))
        }

        sample.exp <- data.frame(Sample = samples,
                                 col = as.hexcol(sample.cols),
                                 Group = factor(groups),
                                 tr.counts)
    } else {
        sample.exp <- NULL
    }

    plotWithTable(plotting.data, sample.exp, display.columns, search.by,
                    id.column=id.column, default.col=cols[2], jitter=jitter,
                    path=path, folder=folder, html=html, launch=launch,
                    table=table, xval=xval, yval=yval,
                    xlab="Mean Expression", ylab="log-fold-change",
                    side.xlab=side.xlab, side.ylab=side.ylab, side.log=side.log,
                    side.gridstep=side.gridstep,
                    ...)

}

convertStatusToCols <- function(x, cols) {
    x <- factor(x, levels=sort(unique(x)))
    output <- cols[x]
    
    output
}

convertColsToHex <- function(cols) {
    if (any(!is.hex(cols))) {
        cols[!is.hex(cols)] <- CharToHexCol(cols[!is.hex(cols)])
    }

    cols
}

makeAnno <- function(x, anno) {
    output <- NULL
    if (is.null(anno) && is.null(x$genes)) {

        warning("No gene annotation provided.")

    } else if (!is.null(anno) && !is.null(x$genes)) {

        anno <- cbind(anno, x$genes)
        anno <- rmDuplicateCols(anno)

    } else if (!is.null(x$genes)) {

        anno <- x$genes

    }

    output <- anno

    output
}

setDisplayColumns <- function(display.columns, anno, xval, yval) {
    if (is.null(display.columns)) {
        display.columns <- names(anno)
        display.columns <- display.columns[display.columns != xval]
        display.columns <- display.columns[display.columns != yval]
    }
    output <- display.columns

    output
}
