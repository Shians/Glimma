# converts status vector to colours
convertStatusToCols <- function(x, cols) {

    isDefaultStates <- function(x) all(x == -1 | x == 0 | x == 1)
    isDefaultCols <- function(x) identical(x, c("#00bfff", "#858585", "#ff3030"))

    if (isDefaultStates(x) && isDefaultCols(cols)) {
        # using default states and colours
        x <- factor(x, levels=c(-1, 0, 1))
    } else {
        # if custom states or colours used
        uniqueStates <- length(unique(x))

        if (uniqueStates != length(cols)) {
            stop("The number of status levels differ from number of supplied colours. ",
                    uniqueStates, " vs ", length(cols), ".")
        }

        status_levels <- sort(unique(x))
        # relevel the input factor and colours such that zero colours are moved to the
        # front, this causes them to be draw first and allows the remaining points to be
        # drawn on top.
        cols <- c(cols[status_levels==0], cols[status_levels!=0])
        cols_levels <- c(0, setdiff(status_levels, 0))
        x <- factor(x, levels=cols_levels)
    }

    if (all(x == 0)) {
        # all insignificant
        output <- rep(cols[2], length(x))
    } else {
        output <- cols[x]
    }

    output
}

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

    output <- make.names(display.columns)

    output
}

# assigns groups as 1:n
initialise_groups <- function(groups, n) {
    if (is.null(groups)) {
        if (not.null(n)) {
            groups <- 1:n
        }
    }

    groups
}

# reorder rows so the background colours are at the top
sortInsigPointsToTop <- function(plotting_data, bg.col) {
    output <- rbind(getRows(plotting_data, plotting_data$cols == bg.col),
                    getRows(plotting_data, plotting_data$cols != bg.col))

    output
}

# transform counts into log-cpm with genes down columns
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

# get indices of rows with NAs in specified columns
naRowInds <- function(res.df, ...) {
    res.df <- data.frame(res.df)
    filterCols <- unlist(list(...))

    delRows <- rep(FALSE, nrow(res.df))

    for (cols in filterCols) {
        delRows <- delRows | is.na(res.df[, cols])
    }

    delRows
}

#' glMDPlot Rmarkdown link and instructions
#'
#' When run inside of a text-block of Rmarkdown document using `r ...` this
#' produces a link and instructions about the usage of the interactive plots.
#'
#' @param html name of the HTML page containing plots from glMDPlot.
#'
#' @seealso \code{\link{glMDPlot}}
#'
#' @return None
#'
#' @examples
#' glMDRmd()
#'
#' @export
glMDRmd <- function(html = "MD-Plot") {
    paste(c(
		paste0("[Click here for interactive version]",
              "(", "glimma-plots/", html , ".html", ")"),
        "",
        "* Hover over points to see sample-wise expression",
        "* Click on column names to sort by column",
        "* Click rows on tables to highlight gene"
    ), collapse = "\n")
}

# create plotting_data data.frame for main plot
get_plotting_data <- function(x, ...) {
    UseMethod("get_plotting_data")
}

get_plotting_data.default <- function(x, anno, cols) {
    if (is.null(anno)) {
        plotting_data <- data.frame(x, cols=cols)
    } else {
        plotting_data <- data.frame(anno, x, cols=cols)
    }
    
    plotting_data
}

get_plotting_data.DGELRT <- function(x, anno, cols, p.adj.method) {
    Adj.PValue <- p.adjust(x$table$PValue, method=p.adj.method)
    x <- data.frame(
        x$table,
        Adj.PValue
    )

    get_plotting_data.default(x, anno, cols)
}

get_plotting_data.DGEExact <- get_plotting_data.DGELRT

get_plotting_data.MArrayLM <- function(x, anno, cols, p.adj.method, coef) {
    Adj.PValue <- stats::p.adjust(x$p.value[, coef], method=p.adj.method)
    x <- data.frame(
        logFC=x$coefficients[, coef],
        logCPM=x$Amean,
        PValue=x$p.value[, coef],
        Adj.PValue=Adj.PValue
    )

    get_plotting_data.default(x, anno, cols)
}

# create sample_exp data.frame for side plot
get_sample_exp <- function(
    counts,
    transform,
    plotting_data,
    side.main,
    groups,
    samples,
    sample.cols
) {
    if (not.null(counts)) {
        transformed_counts <- transformCounts(counts, transform, plotting_data[[side.main]])
        
        if (!is(groups, "numeric")) {
            # reorder samples to group levels
            groups <- factor(groups)
            transformed_counts <- transformed_counts[order(groups), ]
            sample.cols <- sample.cols[order(groups)]
            samples <- samples[order(groups)]
            groups <- sort(groups)
        }

        sample_exp <- data.frame(
            Sample = samples,
            cols = as.hexcol(sample.cols),
            Group = groups,
            transformed_counts
        )
    } else {
        sample_exp <- NULL
    }
    
    sample_exp
}

# create main and side with linkage actions
get_plots_and_links <- function(plotting_data, xval, yval, xlab, main, side.main, ylab, display.columns, counts, side.gridstep, sample_exp, side.xlab, side.ylab, jitter, ...) {
    plot1 <- glScatter(plotting_data, xval=xval, yval=yval,
                       xlab=xlab, main=main, idval=side.main, ylab=ylab,
                       annot=c(display.columns, xval, yval), flag="mdplot",
                       ndigits=4, ...)
    
    if (not.null(counts)) {
        link1 <- gllink(1, 2, "hover", "yChange", flag="byKey", info=side.main)
        link2 <- gllink(1, 2, "click", "yChange", flag="byKey", info=side.main)
        
        if (side.gridstep) {
            plot2 <- glScatter(sample_exp, xval="Group",
                               yval=colnames(sample_exp)[4],
                               xlab=side.xlab, ylab=side.ylab,
                               main=colnames(sample_exp)[4],
                               colval="cols",
                               annot=c("Sample", colnames(sample_exp)[4]),
                               annot.lab=c("Sample", "logCPM"),
                               x.jitter=jitter,
                               ndigits=4, hide=TRUE,
                               ystep=side.gridstep, ygrid=TRUE)
        } else {
            plot2 <- glScatter(sample_exp, xval="Group",
                               yval=colnames(sample_exp)[4],
                               xlab=side.xlab, ylab=side.ylab,
                               main=colnames(sample_exp)[4],
                               colval="cols",
                               annot=c("Sample", colnames(sample_exp)[4]),
                               annot.lab=c("Sample", "logCPM"),
                               x.jitter=jitter,
                               ndigits=4, hide=TRUE, ygrid=FALSE)
        }
    } else {
        link1 <- NULL
        link2 <- NULL
        plot2 <- NULL
    }
    
    list(
        plot1 = plot1,
        plot2 = plot2,
        link1 = link1,
        link2 = link2
    )
}

# make sure side.main column contains unique values
make_side_main_unique <- function(x, anno, side.main) {
    if (not.null(side.main)) {
        if (side.main %in% colnames(x)) {
            x[[side.main]] <- makeUnique(x[[side.main]])
        } else if (side.main %in% colnames(anno)) {
            anno[[side.main]] <- makeUnique(anno[[side.main]])
        }
    }
    
    list(
        x = x,
        anno = anno
    )
}

# get sample size, length if vector and rows if tabular
sample_size <- function(x) {
    samples <- ifelse(
        not.null(nrow(x)),
        nrow(x),
        length(x)
    )

    samples
}

# create anno from count rownames
anno_from_count_rows <- function(anno, counts, side.main) {
    if (is.null(anno)) {
        anno <- data.frame(rownames(counts))
        names(anno) <- side.main
    } else {
        anno <- data.frame(rownames(counts), anno)
        names(anno)[1] <- side.main
    }
    
    anno
}

# assign colnames to samples if samples not defined.
get_samples <- function(samples, counts) {
    if (is.null(samples)) {
        if (not.null(counts)) {
            if (not.null(colnames(counts))) {
                samples <- colnames(counts)
            } else {
                samples <- seq_cols(counts)
            }
        }
    }
    samples
}

# get column of status defined by coef
get_coef_status <- function(status, coef) {
    if (not.null(ncol(status))) {
        if (ncol(status) > 1) {
            status <- status[, coef]
        }
    }
    status
}
