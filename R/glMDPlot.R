#' Glimma MD Plot
#'
#' Draw an interactive MD plot
#'
#' @author Shian Su
#'
#' @param x the DE object to plot.
#' @param ... additional arguments affecting the plots produced. See specific
#'   methods for detailed arguments.
#'
#' @seealso \code{\link{glMDPlot.default}}, \code{\link{glMDPlot.DGELRT}},
#'   \code{\link{glMDPlot.DGEExact}}, \code{\link{glMDPlot.MArrayLM}},
#'   \code{\link{glMDPlot.DESeqDataSet}}
#'
#' @template return_glMDPlot
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
#' @inheritParams glMDPlot
#' @param x the data.frame object containing expression and fold change values.
#' @param xval the column to plot on x axis of left plot.
#' @param yval the column to plot on y axis of left plot.
#' @param counts the matrix of expression values, with samples in columns.
#' @param anno the data.frame containing gene annotations.
#' @param groups the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length
#'   as the number of rows of object. If NULL, then all points are plotted in
#'   the default colour.
#' @param transform TRUE if counts cpm transformed.
#' @param side.main the column containing mains for right plot.
#' @param side.xlab label for x axis on right plot.
#' @param side.ylab label for y axis on right plot.
#' @param side.log TRUE to plot expression on the right plot on log scale.
#' @param side.gridstep intervals along which to place grid lines on y axis.
#'   Currently only available for linear scale.
#' @param xlab the label on the x axis for the left plot.
#' @param ylab the label on the y axis for the left plot.
#' @param jitter the amount of jitter to apply to the samples in the expressions
#'   plot.
#' @param display.columns character vector containing names of columns to
#'   display in mouseover tooltips and table.
#' @param cols vector of strings denoting colours corresponding to control
#'   status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point
#'   on the expression plot.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot. (main, xlab,
#'   ylab can be set for the left plot)
#'
#' @template return_glMDPlot
#'
#' @method glMDPlot default
#'
#' @importFrom stats p.adjust
#' @importFrom methods is
#' @importFrom edgeR cpm
#'
#' @export

glMDPlot.default <- function(x, xval, yval, counts=NULL, anno=NULL,
                        groups=NULL, samples=NULL,
                        status=rep(0, nrow(x)), transform=FALSE,
                        side.main="GeneID",
                        side.xlab="Group", side.ylab="Expression",
                        side.log=FALSE,
                        side.gridstep=ifelse(!transform || side.log, FALSE, 0.5),
                        xlab=xval, ylab=yval, jitter=30,
                        display.columns=side.main,
                        cols=c("#00bfff", "#858585", "#ff3030"),
                        sample.cols=rep("#1f77b4", ncol(counts)),
                        path=getwd(), folder="glimma-plots", html="MD-Plot",
                        launch=TRUE, ...) {

    ##
    # Input checking

    checkThat(length(status), sameAs(nrow(x)))
	checkObjAnnoCountsShapes(anno, counts, x)
    if (not.null(counts)) checkSideMainPresent(side.main, anno, x)

    #
    ##

    ##
    # Value initialisation

    if (side.main %in% colnames(x)) {
        x[[side.main]] <- makeUnique(x[[side.main]])
    } else if (side.main %in% colnames(anno)) {
        anno[[side.main]] <- makeUnique(anno[[side.main]])
    }

    cols <- as.hexcol(cols)

    checkCountsAndSamples(counts, samples, side.log)

    if (is.null(groups)) {
        groups <- initialiseGroups(ncol(counts))
    }

    #
    ##

    jitter <- ifelse(is.numeric(groups), 0, jitter)

    cols <- convertStatusToCols(status, cols)

    if (is.null(anno)) {
        plotting.data <- data.frame(x, cols = cols)
    } else {
        plotting.data <- data.frame(anno, x, cols = cols)
    }

    if (not.null(counts)) {
        tr.counts <- transformCounts(counts, transform, plotting.data[[side.main]])

        if (is(groups, "numeric")) {

            sample.exp <- data.frame(Sample = samples,
                                 cols = as.hexcol(sample.cols),
                                 Group = groups,
                                 tr.counts)

        } else {

            sample.exp <- data.frame(Sample = samples,
                                 cols = as.hexcol(sample.cols),
                                 Group = factor(groups),
                                 tr.counts)

        }
    } else {
        sample.exp <- NULL
    }

    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    plot1 <- glScatter(plotting.data, xval=xval, yval=yval,
                    xlab=xlab, idval=side.main, ylab=ylab,
                    annot=c(display.columns, xval, yval), flag="mdplot",
                    ndigits=4, ...)

    if (not.null(counts)) {

        link1 <- gllink(1, 2, "hover", "yChange", flag="byKey", info=side.main)
        link2 <- gllink(1, 2, "click", "yChange", flag="byKey", info=side.main)

        if (side.gridstep) {
            plot2 <- glScatter(sample.exp, xval="Group",
                                yval=colnames(sample.exp)[4],
                                xlab=side.xlab, ylab=side.ylab,
                                main=colnames(sample.exp)[4],
                                colval="cols",
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
                                colval="cols",
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

    draw.plots(display.columns=display.columns,
                xval=xval,
                yval=yval,
                plot1=plot1,
                plot2=plot2,
                link1=link1,
                link2=link2,
                path=path,
                folder=folder,
                html=html,
                launch=launch)
}

#' Glimma MD Plot
#'
#' Draw an interactive MD plot from a DGELRT object
#'
#' @author Shian Su
#'
#' @inheritParams glMDPlot
#' @param x the DGELRT object.
#' @param counts the matrix of expression values, with samples in columns.
#' @param anno the data.frame containing gene annotations.
#' @param groups the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length
#'   as the number of rows of object. If NULL, then all points are plotted in
#'   the default colour.
#' @param transform TRUE if counts cpm transformed.
#' @param side.main the column containing mains for right plot.
#' @param side.xlab label for x axis on right plot.
#' @param side.ylab label for y axis on right plot.
#' @param side.log TRUE to plot expression on the right plot on log scale.
#' @param side.gridstep intervals along which to place grid lines on y axis.
#'   Currently only available for linear scale.
#' @param p.adj.method character vector indicating multiple testing correction method. See \code{\link{p.adjust}} for available methods. (defaults to "BH")
#' @param jitter the amount of jitter to apply to the samples in the expressions
#'   plot.
#' @param display.columns character vector containing names of columns to
#'   display in mouseover tooltips and table.
#' @param cols vector of strings denoting colours corresponding to control
#'   status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point
#'   on the expression plot.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot. (main, xlab,
#'   ylab can be set for the left plot)
#'
#' @template return_glMDPlot
#'
#' @method glMDPlot DGELRT
#'
#' @importFrom stats p.adjust
#' @importFrom methods is
#'
#' @export

glMDPlot.DGELRT <- function(x, counts=NULL, anno=NULL,
                            groups=NULL, samples=NULL,
                            status=rep(0, nrow(x)), transform=FALSE,
                            side.xlab="Group", side.ylab="Expression",
                            side.log=FALSE,
                            side.gridstep=ifelse(!transform || side.log, FALSE, 0.5),
                            p.adj.method="BH", jitter=30,
                            side.main="GeneID", display.columns=NULL,
                            cols=c("#00bfff", "#858585", "#ff3030"),
                            sample.cols=rep("#1f77b4", ncol(counts)),
                            path=getwd(), folder="glimma-plots", html="MD-Plot",
                            launch=TRUE, ...) {

    ##
    # Input checking

    if (is(status, "numeric")) {
        checkThat(length(status), sameAs(nrow(x)))
    } else if (is(status, "matrix")) {
        checkThat(nrow(status), sameAs(nrow(x)))
    }

    checkObjAnnoCountsShapes(anno, counts, x$table)
    checkCountsAndSamples(counts, samples, side.log)

    # Assign side.main column from rowname of counts if required
    if (not.null(counts) && side.main %!in% union(names(x$table), names(anno))) {
        geneIds <- rownames(counts)
        if (is.null(anno)) {
            anno <- data.frame(geneIds)
            names(anno) <- side.main
        } else {
            anno <- data.frame(geneIds, anno)
            names(anno)[1] <- side.main
        }
    }

    if (not.null(counts)) checkSideMainPresent(side.main, anno, x)

    #
    ##

    ##
    # Value initialisation

    xval <- "logCPM"
    yval <- "logFC"
    anno <- makeAnno(x, anno)
    cols <- as.hexcol(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    if (is.null(samples)) {
        if (not.null(counts)) {
            if (not.null(colnames(counts))) {
                samples <- colnames(counts)
            } else {
                samples <- 1:ncol(counts)
            }
        }
    }

    if (is.null(groups)) {
        groups <- initialiseGroups(ncol(counts))
    }

    jitter <- ifelse(is.numeric(groups), 0, jitter)

    #
    ##

    cols <- convertStatusToCols(status, cols)

    if (is.null(anno)) {

        plotting.data <- data.frame(x$table, cols = cols,
                                    Adj.PValue = stats::p.adjust(x$table$PValue,
                                    method = p.adj.method))

    } else {

        plotting.data <- data.frame(anno, x$table, cols = cols,
                                    Adj.PValue = stats::p.adjust(x$table$PValue,
                                    method = p.adj.method))

    }

    if (not.null(counts)) {
        tr.counts <- transformCounts(counts, transform, plotting.data[[side.main]])

        if (is(groups, "numeric")) {
            sample.exp <- data.frame(Sample = samples,
                                     cols = as.hexcol(sample.cols),
                                     Group = groups,
                                     tr.counts)
        } else {
            sample.exp <- data.frame(Sample = samples,
                                     cols = as.hexcol(sample.cols),
                                     Group = factor(groups),
                                     tr.counts)
        }
    } else {
        sample.exp <- NULL
    }

    plotWithTable(plotting.data, sample.exp, display.columns,
                side.main=side.main, default.col=cols[2], jitter=jitter,
                path=path, folder=folder, html=html, launch=launch,
                xval=xval, yval=yval,
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
#' @inheritParams glMDPlot.DGELRT
#' @param x the DGEExact object.
#'
#' @template return_glMDPlot
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
#' @inheritParams glMDPlot.DGELRT
#' @param x the MArrayLM object.
#' @param coef integer or character index vector indicating which column of object to plot.
#' @param ... additional arguments to be passed onto the MD plot. (main, xlab,
#'   ylab can be set for the left plot)
#'
#' @template return_glMDPlot
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
                            groups=NULL, samples=NULL,
                            status=rep(0, nrow(x)), transform=FALSE,
                            side.main="GeneID",
                            side.xlab="Group", side.ylab="Expression",
                            side.log=FALSE,
                            side.gridstep=ifelse(!transform || side.log, FALSE, 0.5),
                            coef=ncol(x$coefficients),
                            p.adj.method="BH", jitter=30,
                            display.columns=NULL,
                            cols=c("#00bfff", "#858585", "#ff3030"),
                            sample.cols=rep("#1f77b4", ncol(counts)),
                            path=getwd(), folder="glimma-plots", html="MD-Plot",
                            launch=TRUE, ...) {

    ##
    # Input checking

    if (is(status, "numeric")) {
        checkThat(length(status), sameAs(nrow(x)))
    } else if (is(status, "matrix")) {
        checkThat(nrow(status), sameAs(nrow(x)))
    }

    checkObjAnnoCountsShapes(anno, counts, x)
    checkCountsAndSamples(counts, samples, side.log)

    # Assign side.main column from rowname of counts if required
    if (not.null(counts) && side.main %!in% union(names(x), names(anno))) {
        geneIds <- rownames(counts)
        if (is.null(anno)) {
            anno <- data.frame(geneIds)
            names(anno) <- side.main
        } else {
            anno <- data.frame(geneIds, anno)
            names(anno)[1] <- side.main
        }
    }

    if (not.null(counts)) checkSideMainPresent(side.main, anno, x)

    #
    ##

    ##
    # Value initialisation

    xval <- "logCPM"
    yval <- "logFC"
    anno <- makeAnno(x, anno)
    cols <- as.hexcol(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    if (not.null(ncol(status))) {
        if (ncol(status) > 1) {
            status <- status[, coef]
        }
    }

    if (is.null(samples)) {
        if (not.null(counts)) {
            if (not.null(colnames(counts))) {
                samples <- colnames(counts)
            } else {
                samples <- 1:ncol(counts)
            }
        }
    }

    if (is.null(groups)) {
        groups <- initialiseGroups(ncol(counts))
    }

    jitter <- ifelse(is.numeric(groups), 0, jitter)

    #
    ##

    cols <- convertStatusToCols(status, cols)

    Adj.PValue <- stats::p.adjust(x$p.value[, coef], method=p.adj.method)
    if (is.null(anno)) {

        plotting.data <- data.frame(logFC = x$coefficients[, coef],
                                     logCPM = x$Amean,
                                     cols = cols,
                                     PValue = x$p.value[, coef],
                                     Adj.PValue = Adj.PValue)

    } else {

        plotting.data <- data.frame(logFC = x$coefficients[, coef],
                                     logCPM = x$Amean,
                                     cols = cols,
                                     PValue = x$p.value[, coef],
                                     Adj.PValue = Adj.PValue,
                                     anno)

    }

    if (not.null(counts)) {
        tr.counts <- transformCounts(counts, transform, plotting.data[[side.main]])

        if (is(groups, "numeric")) {
            sample.exp <- data.frame(Sample = samples,
                                     cols = as.hexcol(sample.cols),
                                     Group = groups,
                                     tr.counts)
        } else {
            sample.exp <- data.frame(Sample = samples,
                                     cols = as.hexcol(sample.cols),
                                     Group = factor(groups),
                                     tr.counts)
        }
    } else {
        sample.exp <- NULL
    }

    plotWithTable(plotting.data, sample.exp, display.columns,
                side.main=side.main, default.col=cols[2], jitter=jitter,
                path=path, folder=folder, html=html, launch=launch,
                xval=xval, yval=yval,
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
#' @param counts the matrix of expression values, with samples in columns.
#' @param anno the data.frame containing gene annotations.
#' @param groups the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length
#'   as the number of rows of object. If NULL, then all points are plotted in
#'   the default colour.
#' @param transform TRUE if counts cpm transformed.
#' @param side.main the column containing mains for right plot.
#' @param side.xlab label for x axis on right plot.
#' @param side.ylab label for y axis on right plot.
#' @param side.log TRUE to plot expression on the right plot on log scale.
#' @param side.gridstep intervals along which to place grid lines on y axis.
#'   Currently only available for linear scale.
#' @param jitter the amount of jitter to apply to the samples in the expressions
#'   plot.
#' @param display.columns character vector containing names of columns to
#'   display in mouseover tooltips and table.
#' @param cols vector of strings denoting colours corresponding to control
#'   status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point
#'   on the expression plot.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot. (main, xlab,
#'   ylab can be set for the left plot)
#'
#' @template return_glMDPlot
#'
#' @method glMDPlot DESeqDataSet
#'
#' @importFrom methods is
#' @importFrom edgeR cpm
#'
#' @export

glMDPlot.DESeqDataSet <- function(x, counts=NULL, anno, groups, samples,
                                status=rep(0, nrow(x)), transform=FALSE,
                                side.xlab="Group", side.ylab="logMean",
                                side.log=FALSE,
                                side.gridstep=ifelse(!transform || side.log, FALSE, 0.5),
                                jitter=30, side.main="GeneID",
                                display.columns=NULL,
                                cols=c("#00bfff", "#858585", "#ff3030"),
                                sample.cols=rep("#1f77b4", ncol(x)),
                                path=getwd(), folder="glimma-plots",
                                html="MD-Plot", launch=TRUE, ...) {

    if (is.null(counts)) {
        counts <- DESeq2::counts(x)
    }

    ##
    # Input checking

    if (is(status, "numeric")) {
        checkThat(length(status), sameAs(nrow(x)))
    } else if (is(status, "matrix")) {
        checkThat(nrow(status), sameAs(nrow(x)))
    }

    checkObjAnnoCountsShapes(anno, counts, x)
    checkCountsAndSamples(counts, samples, side.log)
    if (not.null(counts)) checkSideMainPresent(side.main, anno, x)

    #
    ##

    ##
    # Value initialisation

    xval <- "logMean"
    yval <- "logFC"

    cols <- as.hexcol(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    #
    ##

    res <- DESeq2::results(x)
    res.df <- as.data.frame(res)
    delRows <- naRowInds(res.df, "log2FoldChange", "padj")

    res.df <- res.df[!delRows, ]
    anno <- anno[!delRows, ]
    counts <- counts[!delRows, ]
    status <- status[!delRows]

    cols <- convertStatusToCols(status, cols)

    plotting.data <- data.frame(logFC = res.df$log2FoldChange,
                                 logMean = log(res.df$baseMean + 0.5),
                                 cols = cols,
                                 PValue = res.df$pvalue,
                                 Adj.PValue = res.df$padj,
                                 anno)

    bg.col <- cols[2]
    plotting.data <- sortInsigPointsToTop(plotting.data, bg.col)

    tr.counts <- transformCounts(counts, transform, plotting.data[[side.main]])

    sample.exp <- data.frame(Sample = samples,
                             cols = as.hexcol(sample.cols),
                             Group = factor(groups),
                             tr.counts)

    plotWithTable(plotting.data, sample.exp, display.columns,
                    side.main=side.main, default.col=cols[2], jitter=jitter,
                    path=path, folder=folder, html=html, launch=launch,
                    xval=xval, yval=yval,
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
#'
#' @template return_glMDPlot
#'
#' @method glMDPlot DESeqResults
#'
#' @importFrom methods is
#' @importFrom edgeR cpm
#'
#' @export

glMDPlot.DESeqResults <- function(x, counts, anno, groups, samples,
                                status=rep(0, nrow(x)), transform=FALSE,
                                side.xlab="Group", side.ylab="Expression",
                                side.log=FALSE,
                                side.gridstep=ifelse(!transform || side.log, FALSE, 0.5),
                                jitter=30, side.main="GeneID",
                                display.columns=NULL,
                                cols=c("#00bfff", "#858585", "#ff3030"),
                                sample.cols=rep("#1f77b4", ncol(counts)),
                                path=getwd(), folder="glimma-plots",
                                html="MD-Plot", launch=TRUE, ...) {

    ##
    # Input checking

    if (is(status, "numeric")) {
        checkThat(length(status), sameAs(nrow(x)))
    } else if (is(status, "matrix")) {
        checkThat(nrow(status), sameAs(nrow(x)))
    }

    checkObjAnnoCountsShapes(anno, counts, x)
    checkCountsAndSamples(counts, samples, side.log)
    if (not.null(counts)) checkSideMainPresent(side.main, anno, x)

    #
    ##

    ##
    # Value initialisation

    xval <- "logMean"
    yval <- "logFC"

    cols <- as.hexcol(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    #
    ##

    res <- x
    res.df <- as.data.frame(res)
    delRows <- naRowInds(res.df, "log2FoldChange", "padj")

    res.df <- res.df[!delRows, ]
    anno <- anno[!delRows, ]
    counts <- counts[!delRows, ]
    status <- status[!delRows]

    cols <- convertStatusToCols(status, cols)

    plotting.data <- data.frame(logFC = res.df$log2FoldChange,
                                 logMean = log(res.df$baseMean + 0.5),
                                 cols = cols,
                                 PValue = res.df$pvalue,
                                 Adj.PValue = res.df$padj,
                                 anno)

    bg.col <- cols[2]
    plotting.data <- sortInsigPointsToTop(plotting.data, bg.col)
    if (not.null(counts)) {

        tr.counts <- transformCounts(counts, transform, plotting.data[[side.main]])

        sample.exp <- data.frame(Sample = samples,
                                 cols = as.hexcol(sample.cols),
                                 Group = factor(groups),
                                 tr.counts)

    } else {
        sample.exp <- NULL
    }

    plotWithTable(plotting.data, sample.exp, display.columns,
                    side.main=side.main, default.col=cols[2], jitter=jitter,
                    path=path, folder=folder, html=html, launch=launch,
                    xval=xval, yval=yval,
                    xlab="Mean Expression", ylab="log-fold-change",
                    side.xlab=side.xlab, side.ylab=side.ylab, side.log=side.log,
                    side.gridstep=side.gridstep,
                    ...)

}

# TODO: Assumption is made that all possible status are present
# need to generalise such no such assumption is made.
convertStatusToCols <- function(x, cols) {
    if (all(x == 0)) {
        output <- rep(cols[2], length(x))
    } else {
        status.levels <- sort(unique(x))
        cols <- c(cols[status.levels==0], cols[status.levels!=0])
        cols.levels <- c(0, setdiff(status.levels, 0))
        x <- factor(x, levels=cols.levels)
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

    output <- display.columns

    output
}

#TODO: Add test
initialiseGroups <- function(n) {
    output <- NULL
    if (not.null(n)) {
        output <- 1:n
    }

    output
}

#TODO: Add test
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

checkSideMainPresent <- function(side.main, anno, x) {
    if (class(x) == "DGELRT" || class(x) == "DGEExact") {
        if (side.main %!in% union(colnames(anno), colnames(x$table))) {
            stop(paste("column", quotify(side.main), "cannot be found in x or anno."))
        }
    } else {
        if (side.main %!in% union(colnames(anno), colnames(x))) {
            stop(paste("column", quotify(side.main), "cannot be found in x or anno."))
        }
    }
}
