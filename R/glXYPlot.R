#' Glimma XY Plot
#'
#' Draw an interactive XY plot with multiple panels
#'
#' @author Charity Law and Shian Su
#'
#' @param x a numeric vector of values to plot on the x-axis of the summary plot.
#' @param y a numeric vector of values to plot on the y-axis of the summary plot.
#' @param counts the matrix containing all counts, the column order should correspond to the order of the x and y vectors.
#' @param groups the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length as the number of rows of object. If NULL, then all points are plotted in the default colour
#' @param anno the data.frame containing gene annotations.
#' @param display.columns character vector containing names of columns to display in mouseover tooltips and table.
#' @param xlab the label on the x axis for the left plot.
#' @param ylab the label on the y axis for the left plot.
#' @param side.main the column containing mains for right plot.
#' @param side.xlab the label on the x axis for the right plot.
#' @param side.ylab the label on the y axis for the right plot.
#' @param sample.cols vector of strings denoting colours for each sample point on the expression plot.
#' @param cols vector of strings denoting colours corresponding to control status -1, 0 and 1. (may be R named colours or Hex values)
#' @param jitter the amount of jitter to apply to the samples in the expressions plot.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
#' @param ... additional arguments to be passed onto the MD plot. (main, etc. can be set for the left plot)
#'
#'
#' @return Draws a two-panel interactive XY scatter plot in an html page. The left plot
#' shows the x and y values specified. The right plot shows the
#' expression levels of a particular gene in each sample. Hovering over points
#' on left plot will plot expression level for the corresponding gene, clicking
#' on points will fix the expression plot to that gene. Clicking on rows on the table
#' has the same effect as clicking on the corresponding gene in the plot. This function
#' generates a display that is similar in style to glMDPlot, except that it provides more
#' flexibility in what the user can provide.
#'
#' @examples
#' data(iris)
#' \donttest{
#' glXYPlot(iris$Sepal.Width, iris$Sepal.Length, 
#'          xlab="Sepal.Width", ylab="Sepal.Length", side.main="PlantID")
#' }
#'
#' @importFrom stats p.adjust
#' @importFrom methods is
#' @importFrom edgeR cpm
#'
#' @export

glXYPlot <- function(
    x,
    y,
    counts = NULL,
    groups = NULL,
    samples = NULL,
    status = rep(0, nrow(data)),
    anno = NULL,
    display.columns = NULL,
    xlab = "x",
    ylab = "y",
    side.main = "GeneID",
    side.xlab = "Group",
    side.ylab = "Expression",
    sample.cols = rep("#1f77b4", length(groups)),
    cols = c("#00bfff", "#858585", "#ff3030"),
    jitter = 30,
    path = getwd(),
    folder = "glimma-plots",
    html = "XY-Plot",
    launch = TRUE,
    ...
) {
    # Plot any x and y
    # Shian Su and Charity Law
    # Created 4 July 2016. Last modified 6 July 2016.

    # Extract values to plot and their names
    checkThat(length(x), sameAs(length(y)))
    data <- cbind(x, y)
    colnames(data) <- c(xlab, ylab)

    # Check input data
    if (not.null(counts)) {
        checkThat(nrow(counts), sameAs(nrow(data)))

        if (is.null(groups)) {
            groups <- as.factor(rep("groups", ncol(counts)))
        }

        if (length(groups) != ncol(counts)) {
            stop("length(groups) not equal to ncol(counts).")
        }

        if (is.null(samples)) {
            samples <- colnames(counts)
        }
    }

    checkThat(length(status), sameAs(length(x)))

    # Check and create annotation
    if (is.null(anno)) {
        if (not.null(counts)) {
            anno <- data.frame(id=rownames(counts))
        } else {
            anno <- data.frame(id=seq_rows(data))
        }
        colnames(anno) <- side.main
    } else {
        anno <- data.frame(anno)
        if (is.null(display.columns)) {
            display.columns <- colnames(anno)
        }
        display.columns <- intersect(display.columns, colnames(anno))

        checkThat(side.main, isIn(display.columns))

        anno <- anno[, colnames(anno) %in% display.columns, drop=FALSE]
    }
    display.columns <- colnames(anno)

    # Remove any colnames in anno that clash with xlab and ylab
    if (any(colnames(x) %in% display.columns)) {
        display.columns <- setdiff(display.columns, colnames(x))
        anno <- anno[, colnames(anno) %in% display.columns, drop=FALSE]
    }

    glMDPlot.default(
        data,
        xval = make.names(xlab),
        yval = make.names(ylab),
        counts = counts,
        groups = groups,
        samples = samples,
        status = status,
        anno = anno,
        display.columns = display.columns,
        side.main = side.main,
        xlab = xlab,
        ylab = ylab,
        side.xlab = side.xlab,
        side.ylab = side.ylab,
        sample.cols = sample.cols,
        cols = cols,
        jitter = jitter,
        table = TRUE,
        path = path,
        folder = folder,
        html = html,
        launch = launch,
        ...
    )
}
