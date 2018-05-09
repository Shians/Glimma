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
    if ("id.column" %in% names(list(...))) stop("argument 'id.column' has been deprecated, please use 'side.main' instead")
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
#' @param transform TRUE if counts should be log-cpm transformed.
#' @param main the title for the left plot.
#' @param xlab the label on the x axis for the left plot.
#' @param ylab the label on the y axis for the left plot.
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
#' @method glMDPlot default
#'
#' @importFrom stats p.adjust
#' @importFrom methods is
#' @importFrom edgeR cpm
#'
#' @export

glMDPlot.default <- function(
    x,
    xval,
    yval,
    counts = NULL,
    anno = NULL,
    groups = NULL,
    samples = NULL,
    status = rep(0, nrow(x)),
    transform = FALSE,
    main = "",
    xlab = xval,
    ylab = yval,
    side.main = "GeneID",
    side.xlab = "Group",
    side.ylab = "Expression",
    side.log = FALSE,
    side.gridstep = ifelse(!transform || side.log, FALSE, 0.5),
    jitter = 30,
    display.columns = side.main,
    cols = c("#00bfff", "#858585", "#ff3030"),
    sample.cols = rep("#1f77b4", ncol(counts)),
    path = getwd(),
    folder = "glimma-plots",
    html = "MD-Plot",
    launch = TRUE,
    ...
) {
    # check status has same length as number of genes
    checkThat(sample_size(status), sameAs(nrow(x)))
    checkObjAnnoCountsShapes(anno, counts, x)

    if (not.null(counts)) {
        # if counts present, check we have valid side.main
        checkSideMainPresent(side.main, anno, x)
    } else {
        # else it does not matter
        side.main <- NULL
    }

    # append numbers to duplicated values
    x <- make_side_main_unique(x, anno, side.main)$x
    anno <- make_side_main_unique(x, anno, side.main)$anno

    checkCountsAndSamples(counts, samples, side.log)

    groups <- initialise_groups(groups, ncol(counts))
    jitter <- ifelse(is.numeric(groups), 0, jitter)
    cols <- convertStatusToCols(status, as.hexcol(cols))

    plotting_data <- get_plotting_data(x, anno, cols)
    sample_exp <- get_sample_exp(counts, transform, plotting_data, side.main, groups, samples, sample.cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    plots_and_links <- get_plots_and_links(plotting_data, xval, yval, xlab, main, side.main, ylab, display.columns, counts, side.gridstep, sample_exp, side.xlab, side.ylab, jitter, ...)

    plot1 <- plots_and_links$plot1
    plot2 <- plots_and_links$plot2
    link1 <- plots_and_links$link1
    link2 <- plots_and_links$link2

    draw.plots(
        display.columns = display.columns,
        xval = xval,
        yval = yval,
        plot1 = plot1,
        plot2 = plot2,
        link1 = link1,
        link2 = link2,
        path = path,
        folder = folder,
        html = html,
        launch = launch
    )
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
#' @param transform TRUE if counts should be log-cpm transformed.
#' @param main the title for the left plot.
#' @param xlab label for x axis on left plot.
#' @param ylab label for y axis on left plot.
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

glMDPlot.DGELRT <- function(
    x,
    counts = NULL,
    anno = NULL,
    groups = NULL,
    samples = NULL,
    status = rep(0, nrow(x)),
    transform = FALSE,
    main = "",
    xlab = "Average log CPM",
    ylab = "log-fold-change",
    side.xlab = "Group",
    side.ylab = "Expression",
    side.log = FALSE,
    side.gridstep = ifelse(!transform || side.log, FALSE, 0.5),
    p.adj.method = "BH",
    jitter = 30,
    side.main = "GeneID",
    display.columns = NULL,
    cols = c("#00bfff", "#858585", "#ff3030"),
    sample.cols = rep("#1f77b4", ncol(counts)),
    path = getwd(),
    folder = "glimma-plots",
    html = "MD-Plot",
    launch = TRUE,
    ...
) {

    # check status has same length as number of genes
    checkThat(sample_size(status), sameAs(sample_size(x)))
    checkObjAnnoCountsShapes(anno, counts, x$table)
    checkCountsAndSamples(counts, samples, side.log)

    anno <- makeAnno(x, anno)
    # Assign side.main column from rowname of counts if required
    missing_side_main <- side.main %!in% union(names(x$table), names(anno))
    if (not.null(counts) && missing_side_main) {
        anno <- anno_from_count_rows(anno, counts, side.main)
    }

    if (not.null(counts)) {
        # if counts present, check we have valid side.main
        checkSideMainPresent(side.main, anno, x)
    } else {
        # else it does not matter
        side.main <- NULL
    }

    xval <- "logCPM"
    yval <- "logFC"
    cols <- as.hexcol(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    # append numbers to duplicated values
    x <- make_side_main_unique(x, anno, side.main)$x
    anno <- make_side_main_unique(x, anno, side.main)$anno

    samples <- get_samples(samples, counts)
    groups <- initialise_groups(groups, ncol(counts))
    jitter <- ifelse(is.numeric(groups), 0, jitter)
    cols <- convertStatusToCols(status, cols)

    plotting_data <- get_plotting_data(x, anno, cols, p.adj.method)
    sample_exp <- get_sample_exp(counts, transform, plotting_data, side.main, groups, samples, sample.cols)

    plotWithTable(
        plotting_data,
        sample_exp,
        display.columns,
        main=main,
        side.main = side.main,
        default.col = cols[2],
        jitter = jitter,
        path = path,
        folder = folder,
        html = html,
        launch = launch,
        xval = xval,
        yval = yval,
        xlab = xlab,
        ylab = ylab,
        side.xlab = side.xlab,
        side.ylab = side.ylab,
        side.log = side.log,
        side.gridstep = side.gridstep,
        ...
    )
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

glMDPlot.MArrayLM <- function(
    x,
    counts = NULL,
    anno = NULL,
    groups = NULL,
    samples = NULL,
    status = rep(0, nrow(x)),
    transform = FALSE,
    main = "",
    xlab = "Average log CPM",
    ylab = "log-fold-change",
    side.main = "GeneID",
    side.xlab = "Group",
    side.ylab = "Expression",
    side.log = FALSE,
    side.gridstep = ifelse(!transform || side.log, FALSE, 0.5),
    coef = ncol(x$coefficients),
    p.adj.method = "BH",
    jitter = 30,
    display.columns = NULL,
    cols = c("#00bfff", "#858585", "#ff3030"),
    sample.cols = rep("#1f77b4", ncol(counts)),
    path = getwd(),
    folder = "glimma-plots",
    html = "MD-Plot",
    launch = TRUE,
    ...
) {

    # check status has same length as number of genes
    checkThat(sample_size(status), sameAs(nrow(x)))
    checkObjAnnoCountsShapes(anno, counts, x)
    checkCountsAndSamples(counts, samples, side.log)

    anno <- makeAnno(x, anno)
    # Assign side.main column from rowname of counts if required
    missing_side_main <- side.main %!in% union(names(x$table), names(anno))
    if (not.null(counts) && missing_side_main) {
        anno <- anno_from_count_rows(anno, counts, side.main)
    }

    if (not.null(counts)) {
        # if counts present, check we have valid side.main
        checkSideMainPresent(side.main, anno, x)
    } else {
        # else it does not matter
        side.main <- NULL
    }

    xval <- "logCPM"
    yval <- "logFC"
    cols <- as.hexcol(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    # append numbers to duplicated values
    x <- make_side_main_unique(x, anno, side.main)$x
    anno <- make_side_main_unique(x, anno, side.main)$anno
    status <- get_coef_status(status, coef)

    samples <- get_samples(samples, counts)
    groups <- initialise_groups(groups, ncol(counts))
    jitter <- ifelse(is.numeric(groups), 0, jitter)
    cols <- convertStatusToCols(status, cols)

    plotting_data <- get_plotting_data(x, anno, cols, p.adj.method, coef)
    sample_exp <- get_sample_exp(counts, transform, plotting_data, side.main, groups, samples, sample.cols)

    plotWithTable(plotting_data,
        sample_exp,
        display.columns,
        main=main,
        side.main = side.main,
        default.col = cols[2],
        jitter = jitter,
        path = path,
        folder = folder,
        html = html,
        launch = launch,
        xval = xval,
        yval = yval,
        xlab = xlab,
        ylab = ylab,
        side.xlab = side.xlab,
        side.ylab = side.ylab,
        side.log = side.log,
        side.gridstep = side.gridstep,
        ...
    )
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
#' @param transform TRUE if counts should be log-cpm transformed.
#' @param main the title for the left plot.
#' @param xlab label for x axis on left plot.
#' @param ylab label for y axis on left plot.
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

glMDPlot.DESeqDataSet <- function(
    x,
    counts = NULL,
    anno,
    groups,
    samples = NULL,
    status = rep(0, nrow(x)),
    transform = FALSE,
    main = "",
    xlab = "Mean Expression",
    ylab = "log-fold-change",
    side.xlab = "Group",
    side.ylab = "logMean",
    side.log = FALSE,
    side.gridstep = ifelse(!transform || side.log, FALSE, 0.5),
    jitter = 30,
    side.main = "GeneID",
    display.columns = NULL,
    cols = c("#00bfff", "#858585", "#ff3030"),
    sample.cols = rep("#1f77b4", ncol(x)),
    path = getwd(),
    folder = "glimma-plots",
    html = "MD-Plot",
    launch = TRUE,
    ...
) {

    if (is.null(counts)) {
        counts <- DESeq2::counts(x)
    }

    ##
    # Input checking

    # check status has same length as number of genes
    if (is(status, "numeric")) {
        checkThat(length(status), sameAs(nrow(x)))
    } else if (is(status, "matrix")) {
        checkThat(nrow(status), sameAs(nrow(x)))
    }

    checkObjAnnoCountsShapes(anno, counts, x)
    checkCountsAndSamples(counts, samples, side.log)

    if (not.null(counts)) {
        checkSideMainPresent(side.main, anno, x)
    } else {
        side.main <- NULL
    }

    #
    ##

    ##
    # Value initialisation

    xval <- "logMean"
    yval <- "logFC"

    # append numbers to duplicated values
    if (not.null(side.main)) {
        if (side.main %in% colnames(x)) {
            x[[side.main]] <- makeUnique(x[[side.main]])
        } else if (side.main %in% colnames(anno)) {
            anno[[side.main]] <- makeUnique(anno[[side.main]])
        }
    }

    cols <- as.hexcol(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    if (is.null(samples)) {
        samples <- colnames(counts)
    }

    #
    ##

    res <- DESeq2::results(x)
    res.df <- as.data.frame(res)
    delRows <- naRowInds(res.df, "log2FoldChange", "padj")

    res.df <- getRows(res.df, !delRows)
    anno <- getRows(anno, !delRows)
    counts <- getRows(counts, !delRows)
    status <- status[!delRows]

    cols <- convertStatusToCols(status, cols)

    plotting_data <- data.frame(
        logFC=res.df$log2FoldChange,
        logMean=log(res.df$baseMean + 0.5),
        cols=cols,
        PValue=res.df$pvalue,
        Adj.PValue=res.df$padj,
        anno
    )

    bg.col <- cols[2]

    tr.counts <- transformCounts(counts, transform, plotting_data[[side.main]])

    plotting_data <- sortInsigPointsToTop(plotting_data, bg.col)

    sample_exp <- data.frame(
        Sample=samples,
        cols=as.hexcol(sample.cols),
        Group=factor(groups),
        tr.counts
    )

    plotWithTable(
        plotting_data,
        sample_exp,
        display.columns,
        main=main,
        side.main = side.main,
        default.col = cols[2],
        jitter = jitter,
        path = path,
        folder = folder,
        html = html,
        launch = launch,
        xval = xval,
        yval = yval,
        xlab = xlab,
        ylab = ylab,
        side.xlab = side.xlab,
        side.ylab = side.ylab,
        side.log = side.log,
        side.gridstep = side.gridstep,
        ...
    )
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

glMDPlot.DESeqResults <- function(
    x,
    counts = NULL,
    anno,
    groups,
    samples = NULL,
    status = rep(0, nrow(x)),
    transform = FALSE,
    main = "",
    xlab = "Mean Expression",
    ylab = "log-fold-change",
    side.xlab = "Group",
    side.ylab = "Expression",
    side.log = FALSE,
    side.gridstep = ifelse(!transform || side.log, FALSE, 0.5),
    jitter = 30,
    side.main = "GeneID",
    display.columns = NULL,
    cols = c("#00bfff", "#858585", "#ff3030"),
    sample.cols = rep("#1f77b4", ncol(counts)),
    path = getwd(),
    folder = "glimma-plots",
    html = "MD-Plot",
    launch = TRUE,
    ...
) {

    ##
    # Input checking

    # check status has same length as number of genes
    if (is(status, "numeric")) {
        checkThat(length(status), sameAs(nrow(x)))
    } else if (is(status, "matrix")) {
        checkThat(nrow(status), sameAs(nrow(x)))
    }

    checkObjAnnoCountsShapes(anno, counts, x)
    checkCountsAndSamples(counts, samples, side.log)
    if (not.null(counts)) {
        checkSideMainPresent(side.main, anno, x)
    } else {
        side.main <- NULL
    }

    #
    ##

    ##
    # Value initialisation

    xval <- "logMean"
    yval <- "logFC"

    # append numbers to duplicated values
    if (not.null(side.main)) {
        if (side.main %in% colnames(x)) {
            x[[side.main]] <- makeUnique(x[[side.main]])
        } else if (side.main %in% colnames(anno)) {
            anno[[side.main]] <- makeUnique(anno[[side.main]])
        }
    }

    cols <- as.hexcol(cols)
    display.columns <- setDisplayColumns(display.columns, anno, xval, yval)

    if (is.null(samples)) {
        samples <- colnames(counts)
    }

    #
    ##

    res <- x
    res.df <- as.data.frame(res)
    delRows <- naRowInds(res.df, "log2FoldChange", "padj")

    res.df <- getRows(res.df, !delRows)
    anno <- getRows(anno, !delRows)
    counts <- getRows(counts, !delRows)
    status <- status[!delRows]

    cols <- convertStatusToCols(status, cols)

    plotting_data <- data.frame(
        logFC=res.df$log2FoldChange,
        logMean=log(res.df$baseMean + 0.5),
        cols=cols,
        PValue=res.df$pvalue,
        Adj.PValue=res.df$padj,
        anno
    )

    bg.col <- cols[2]

    if (not.null(counts)) {

        tr.counts <- transformCounts(counts, transform, plotting_data[[side.main]])

        sample_exp <- data.frame(
            Sample=samples,
            cols=as.hexcol(sample.cols),
            Group=factor(groups),
            tr.counts
        )

    } else {
        sample_exp <- NULL
    }

    plotting_data <- sortInsigPointsToTop(plotting_data, bg.col)

    plotWithTable(
        plotting_data,
        sample_exp,
        display.columns,
        main=main,
        side.main=side.main,
        default.col=cols[2],
        jitter=jitter,
        path=path,
        folder=folder,
        html=html,
        launch=launch,
        xval=xval,
        yval=yval,
        xlab=xlab,
        ylab=ylab,
        side.xlab=side.xlab,
        side.ylab=side.ylab,
        side.log=side.log,
        side.gridstep=side.gridstep,
        ...
    )

}
