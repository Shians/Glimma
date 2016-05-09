#' Glimma MD Plot
#'
#' Draw an interactive MD plot
#'
#' @author Shian Su
#'
#' @param x the data.frame containing data to plot.
#' @param ... additional arguments affecting the plots produced. See specific methods for detailed arguments.
#'
#' @seealso \code{\link{glMDPlot.default}}, \code{\link{glMDPlot.DGELRT}}, \code{\link{glMDPlot.DGEExact}}, \code{\link{glMDPlot.MArrayLM}}, \code{\link{glMDPlot.DESeqDataSet}}
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot shows the log-fold-change vs average expression. The right plot shows the expression levels of a particular gene of each sample.
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

# Hidden internal functions for use by edgeR and limma based plotting
glMDPlot.hidden <- function(plotting.data, sample.exp, display.columns,
                            search.by, id.column, default.col, jitter,
                            table, path, folder, html, launch,
                            xval, yval, xlab, ylab, ...) {

    # Reordering so that significant points appear on top of insignificant
    # points.
    plotting.data <- rbind(plotting.data[plotting.data$col == default.col, ],
                           plotting.data[plotting.data$col != default.col, ])

    plot1 <- glScatter(plotting.data, xval=xval, yval=yval,
                    xlab=xlab, idval=id.column,
                    ylab=ylab,
                    annot=c(display.columns, xval, yval, "Adj.PValue"),
                    flag="mdplot", ndigits=4, info=list(search.by=search.by),
                    ...)

    plot2 <- glScatter(sample.exp, xval="Group", yval=colnames(sample.exp)[4],
                    idval="Sample", ylab="logCPM", main=colnames(sample.exp)[4],
                    annot=c("Sample", colnames(sample.exp)[4]),
                    colval="col",
                    annot.lab=c("Sample", "logCPM"), x.jitter = jitter,
                    ndigits=4, hide=TRUE)

    link1 <- gllink(1, 2, "hover", "yChange", flag="byKey", info=id.column)
    link2 <- gllink(1, 2, "click", "yChange", flag="byKey", info=id.column)

    draw.plots(table, display.columns, search.by, xval, yval,
                plot1, plot2, link1, link2, path, folder, html,
                launch)
}

draw.plots <- function(table, display.columns, search.by, xval, yval,
                        plot1, plot2, link1, link2, path, folder, html,
                        launch) {
    if (table) {
        # TODO: Have different columns to tooltip
        link3 <- gltablink(1, 1, action="highlightById")
        table1 <- glTable(1, c(display.columns, xval, yval, "Adj.PValue"))
        glimma(plot1, plot2, link1, link2, table1, link3, layout=c(1, 2),
            path=path, folder=folder, html=html, overwrite=TRUE, launch=launch)
    } else {
        button1 <- glAutoinput(1, "highlightBySearch", search.by)
        glimma(plot1, plot2, button1, link1, link2, layout=c(1, 2),
            path=path, folder=folder, html=html, overwrite=TRUE, launch=launch)
    }
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
#' @param xlab the label on the x axis for the left plot.
#' @param ylab the label on the y axis for the left plot.
#' @param side.xlab the label on the x axis for the right plot.
#' @param side.ylab the label on the y axis for the right plot.
#' @param search.by the name of the column which will be used to search for data points if table is not used. (should contain unique values)
#' @param jitter the amount of jitter to apply to the samples in the expressions plot.
#' @param id.column the column containing unique identifiers for each gene.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point on the expression plot.
#' @param table logical variable for whether a table of the data should appear on the bottom of the HTML page.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot.
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot shows the log-fold-change vs average expression. The right plot shows the expression levels of a particular gene of each sample.
#'
#' @method glMDPlot default
#'
#' @importFrom stats p.adjust
#' @importFrom methods is
#' @importFrom edgeR cpm
#'
#' @export

glMDPlot.default <- function(x, xval, yval, counts, anno, groups, samples,
                        status=rep(0, nrow(x)), xlab=xval, ylab=yval,
                        side.xlab="Group", side.ylab="logCPM",
                        search.by="Symbols", jitter=30,
                        id.column="GeneID", display.columns=id.column,
                        cols=c("#0000FF", "#858585", "#B32222"),
                        sample.cols=rep("#1f77b4", ncol(counts)),
                        table=TRUE,
                        path=getwd(), folder="glimma-plots", html="MD-Plot",
                        launch=TRUE, ...) {

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

    plotting.data <- data.frame(anno, x, col = col)

    rownames(counts) <- make.names(plotting.data[[id.column]])

    if (is(groups, "numeric")) {
        sample.exp <- data.frame(Sample = samples,
                             col = as.hexcol(sample.cols),
                             Group = groups,
                             t(cpm(as.matrix(counts), log=TRUE)))
    } else {
        sample.exp <- data.frame(Sample = samples,
                             col = as.hexcol(sample.cols),
                             Group = factor(groups),
                             t(cpm(as.matrix(counts), log=TRUE)))
    }

    # Reordering so that significant points appear on top of insignificant
    # points.

    plotting.data <- rbind(plotting.data[plotting.data$col == cols[2], ],
                           plotting.data[plotting.data$col != cols[2], ])

    plot1 <- glScatter(plotting.data, xval=xval, yval=yval,
                    xlab=xlab, idval=id.column, ylab=ylab,
                    annot=c(display.columns, xval, yval), flag="mdplot",
                    ndigits=4, info=list(search.by=search.by), ...)

    plot2 <- glScatter(sample.exp, xval="Group", yval=colnames(sample.exp)[4],
                        xlab=side.xlab, ylab=side.ylab,
                        main=colnames(sample.exp)[4],
                        colval="col",
                        annot=c("Sample", colnames(sample.exp)[4]),
                        annot.lab=c("Sample", "logCPM"), x.jitter = jitter,
                        ndigits=4, hide=TRUE)

    link1 <- gllink(1, 2, "hover", "yChange", flag="byKey", info=id.column)
    link2 <- gllink(1, 2, "click", "yChange", flag="byKey", info=id.column)

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
#' @param coef integer or character index vector indicating which column of object to plot.
#' @param p.adj.method character vector indicating multiple testing correction method. (defaults to "BH")
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param jitter the amount of jitter to apply to the samples in the expressions plot.
#' @param id.column the column containing unique identifiers for each gene.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point on the expression plot.
#' @param table logical variable for whether a table of the data should appear on the bottom of the HTML page.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot.
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot shows the log-fold-change vs average expression. The right plot shows the expression levels of a particular gene of each sample.
#'
#' @method glMDPlot DGELRT
#'
#' @importFrom stats p.adjust
#' @importFrom methods is
#'
#' @export

glMDPlot.DGELRT <- function(x, counts, anno, groups, samples,
                            status=rep(0, nrow(x)), coef=ncol(x$coefficients),
                            p.adj.method="BH", search.by="Symbols", jitter=30,
                            id.column="GeneID", display.columns=id.column,
                            cols=c("#0000FF", "#858585", "#B32222"),
                            sample.cols=rep("#1f77b4", ncol(counts)),
                            table=TRUE,
                            path=getwd(), folder="glimma-plots", html="MD-Plot",
                            launch=TRUE, ...) {

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
                                Adj.PValue = p.adjust(x$table$PValue,
                                method = p.adj.method))

    rownames(counts) <- make.names(plotting.data[[id.column]])

    sample.exp <- data.frame(Sample = samples,
                             col = as.hexcol(sample.cols),
                             Group = factor(groups),
                             t(cpm(as.matrix(counts), log=TRUE)))

    glMDPlot.hidden(plotting.data, sample.exp, display.columns, search.by,
                id.column=id.column, default.col=cols[2], jitter=jitter,
                path=path, folder=folder, html=html, launch=launch,
                table=table, xval="logCPM", yval="logFC",
                xlab="Average log CPM", ylab="log-fold-change", ...)
}

#' Glimma MD Plot
#'
#' Draw an interactive MD plot from a DGELRT objet
#'
#' @author Shian Su
#'
#' @param x the DGEExact object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param groups the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length as the number of rows of object. If NULL, then all points are plotted in the default colour.
#' @param coef integer or character index vector indicating which column of object to plot.
#' @param p.adj.method character vector indicating multiple testing correction method. (defaults to "BH")
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param jitter the amount of jitter to apply to the samples in the expressions plot.
#' @param id.column the column containing unique identifiers for each gene.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point on the expression plot.
#' @param table logical variable for whether a table of the data should appear on the bottom of the HTML page.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot.
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot shows the log-fold-change vs average expression. The right plot shows the expression levels of a particular gene of each sample.
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
#' @param coef integer or character index vector indicating which column of object to plot.
#' @param p.adj.method character vector indicating multiple testing correction method. (defaults to "BH")
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param jitter the amount of jitter to apply to the samples in the expressions plot.
#' @param id.column the column containing unique identifiers for each gene.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point on the expression plot.
#' @param table logical variable for whether a table of the data should appear on the bottom of the HTML page.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot.
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot shows the log-fold-change vs average expression. The right plot shows the expression levels of a particular gene of each sample.
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

glMDPlot.MArrayLM <- function(x, counts, anno, groups, samples,
                            status=rep(0, nrow(x)), coef=ncol(x$coefficients),
                            p.adj.method="BH", search.by="Symbols", jitter=30,
                            id.column="GeneID", display.columns=id.column,
                            cols=c("#0000FF", "#858585", "#B32222"),
                            sample.cols=rep("#1f77b4", ncol(counts)),
                            table=TRUE,
                            path=getwd(), folder="glimma-plots", html="MD-Plot",
                            launch=TRUE, ...) {

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

    Adj.PValue <- p.adjust(x$p.value[, coef], method=p.adj.method)
    plotting.data <- data.frame(logFC = x$coefficients[, coef],
                                 logCPM = x$Amean,
                                 col = col,
                                 PValue = x$p.value[, coef],
                                 Adj.PValue = Adj.PValue,
                                 anno)

    rownames(counts) <- make.names(plotting.data[[id.column]])

    sample.exp <- data.frame(Sample = samples,
                             col = as.hexcol(sample.cols),
                             Group = factor(groups),
                             t(cpm(as.matrix(counts), log=TRUE)))

    glMDPlot.hidden(plotting.data, sample.exp, display.columns, search.by,
                id.column=id.column, default.col=cols[2], jitter=jitter,
                path=path, folder=folder, html=html, launch=launch,
                table=table, xval="logCPM", yval="logFC",
                xlab="Average log CPM", ylab="log-fold-change", ...)
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
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param jitter the amount of jitter to apply to the samples in the expressions plot.
#' @param id.column the column containing unique identifiers for each gene.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point on the expression plot.
#' @param table logical variable for whether a table of the data should appear on the bottom of the HTML page.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot.
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot shows the log-fold-change vs average expression. The right plot shows the expression levels of a particular gene of each sample.
#'
#' @method glMDPlot DESeqDataSet
#'
#' @importFrom methods is
#' @importFrom edgeR cpm
#' @importFrom DESeq2 counts results DESeqDataSet
#'
#' @export

glMDPlot.DESeqDataSet <- function(x, anno, groups, samples,
                                status=rep(0, nrow(x)), search.by="Symbols",
                                jitter=30, id.column="GeneID",
                                display.columns=id.column,
                                cols=c("#0000FF", "#858585", "#B32222"),
                                sample.cols=rep("#1f77b4", ncol(x)),
                                table=TRUE,
                                path=getwd(), folder="glimma-plots",
                                html="MD-Plot", launch=TRUE, ...) {

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
    gene.counts <- counts(x)

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

    # Reordering so that significant points appear on top of insignificant
    # points.
    plotting.data <- rbind(plotting.data[plotting.data$col == cols[2], ],
                           plotting.data[plotting.data$col != cols[2], ])

    rownames(gene.counts) <- make.names(plotting.data[[id.column]])

    sample.exp <- data.frame(Sample = samples,
                             col = as.hexcol(sample.cols),
                             Group = factor(groups),
                             t(cpm(gene.counts, log=TRUE)))

    glMDPlot.hidden(plotting.data, sample.exp, display.columns, search.by,
                    id.column=id.column, default.col=cols[2], jitter=jitter,
                    path=path, folder=folder, html=html, launch=launch,
                    table=table, xval="logMean", yval="logFC",
                    xlab="Mean Expression", ylab="log-fold-change", ...)
}

#' Glimma MD Plot
#'
#' Draw an interactive MD plot from a DESeqResults object
#'
#' @author Shian Su
#'
#' @param x the DESeqResults object.
#' @param counts the matrix containing all counts.
#' @param anno the data.frame containing gene annotations.
#' @param groups the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length as the number of rows of object. If NULL, then all points are plotted in the default colour.
#' @param search.by the name of the column which will be used to search for data points. (should contain unique values)
#' @param jitter the amount of jitter to apply to the samples in the expressions plot.
#' @param id.column the column containing unique identifiers for each gene.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point on the expression plot.
#' @param table logical variable for whether a table of the data should appear on the bottom of the HTML page.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot.
#'
#' @return Draws a two-panel interactive MD plot in an html page. The left plot shows the log-fold-change vs average expression. The right plot shows the expression levels of a particular gene of each sample.
#'
#' @method glMDPlot DESeqResults
#'
#' @importFrom methods is
#' @importFrom edgeR cpm
#' @importFrom DESeq2 counts results DESeqResults
#'
#' @export

glMDPlot.DESeqResults <- function(x, counts, anno, groups, samples,
                                status=rep(0, nrow(x)), search.by="Symbols",
                                jitter=30, id.column="GeneID",
                                display.columns=id.column,
                                cols=c("#0000FF", "#858585", "#B32222"),
                                sample.cols=rep("#1f77b4", ncol(counts)),
                                table=TRUE,
                                path=getwd(), folder="glimma-plots",
                                html="MD-Plot", launch=TRUE, ...) {

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

    res <- x
    res.df <- as.data.frame(res)
    gene.counts <- counts

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

    # Reordering so that significant points appear on top of insignificant points.
    plotting.data <- rbind(plotting.data[plotting.data$col == cols[2], ],
                           plotting.data[plotting.data$col != cols[2], ])

    rownames(gene.counts) <- make.names(plotting.data[[id.column]])

    sample.exp <- data.frame(Sample = samples,
                             col = as.hexcol(sample.cols),
                             Group = factor(groups),
                             t(cpm(gene.counts, log=TRUE)))

    glMDPlot.hidden(plotting.data, sample.exp, display.columns, search.by,
                    id.column=id.column, default.col=cols[2], jitter=jitter,
                    path=path, folder=folder, html=html, launch=launch,
                    table=table, xval="logMean", yval="logFC",
                    xlab="Mean Expression", ylab="log-fold-change", ...)
}
