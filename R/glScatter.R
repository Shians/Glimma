#' Glimma Scatter Plot
#'
#' Create an interactive scatter plot object
#'
#' @author Shian Su
#'
#' @param x the data.frame containing data to plot.
#' @param ... additional arguments depending on input object type.
#'
#' @return A chart object containing the information to create an interactive scatter plot.
#'
#' @examples
#' data(iris)
#' \donttest{
#' plot1 <- glScatter(iris, xval="Sepal.Length", yval="Sepal.Width", colval="Species")
#' glimma(plot1, c(1,1))
#' }

glScatter <- function(x, ...) {
    UseMethod("glScatter")
}

#' Glimma Scatter Plot
#'
#' Default method for creating an interactive scatter plot
#'
#' @author Shian Su
#'
#' @param x the data.frame containing data to plot.
#' @param xval the column name for the x-axis values.
#' @param yval the column name for the y-axis values.
#' @param idval the column name for unique identifiers.
#' @param point.size the size of the data points.
#' @param x.jitter the amount of jittering to add to values along the x axis.
#' @param y.jitter the amount of jittering to add to values along the y axis.
#' @param ndigits the number of digits after the decimal to round to in the tooltip (overrides signif).
#' @param signif the number of significant figures to display in the tooltip.
#' @param log a character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic.
#' @param xgrid TRUE if grid lines should be placed along x axis.
#' @param ygrid TRUE if grid lines should be placed y axis.
#' @param xstep the interval at which to set grid lines along the x axis.
#' @param ystep the interval at which to set grid lines along the y axis.
#' @param xlab the label on the x-axis.
#' @param ylab the label on the y-axis.
#' @param main the title for the plot.
#' @param height the height of the plot (in pixels).
#' @param width the width of the plot (in pixels).
#' @param colval the colours for each data point.
#' @param annot the columns to display in the tooltip.
#' @param annot.lab alternative labels for the values displayed in the tooltip.
#' @param flag the special flag to indicate special plot.
#' @param info additional information for plotting.
#' @param hide TRUE to hide the plot when page starts.
#' @param disable the events to disable, options are "click", "hover", "zoom".
#' @param ... additional arguments.
#'
#' @return A chart object containing the information to create an interactive scatter plot.
#'
#' @examples
#' data(iris)
#' \donttest{
#' plot1 <- glScatter(iris, xval="Sepal.Length", yval="Sepal.Width", colval="Species")
#' glimma(plot1, c(1,1))
#' }

glScatter.default <- function(x, xval="x", yval="y", idval=NULL, point.size=2,
                                x.jitter = 0, y.jitter = 0,
                                ndigits=NULL, signif=6, log="",
                                xgrid=FALSE, ygrid=FALSE,
                                xstep=FALSE, ystep=FALSE,
                                xlab=xval, ylab=yval, main=NULL,
                                height=400, width=500,
                                colval=NULL, annot=c(xval, yval),
                                annot.lab=NULL, flag=NULL, info=NULL,
                                hide=FALSE, disable=NULL, ...) {
    ##
    # Input checking
    checkThat(xval, isCharacter)
    checkThat(yval, isCharacter)
    checkThat(annot, isCharacter)

    checkThat(xval, isIn(names(x)))
    checkThat(yval, isIn(names(x)))

    if (not.null(colval)) {
        checkThat(colval, isIn(names(x)))
    }

    if (not.null(idval)) {
        checkThat(idval, isIn(names(x)))
    }

    if (any(is.na(match(annot, names(x))))) {
        stop(paste("not all values in annot correspond to a column"))
    }
    #
    ##

    # Normalise input
    x <- data.frame(x)

    # Make json out of data
    json <- makeJson(x, convert.logical=FALSE, dataframe="column")

    x.ord <- is.factor(x[[xval]])
    y.ord <- is.factor(x[[yval]])
    x.log <- "x" %in% unlist(strsplit(log, NULL))
    y.log <- "y" %in% unlist(strsplit(log, NULL))

    if (not.null(colval)) {
        if (all(is.hex(x[[colval]]))) {
            cfixed <- TRUE
        } else {
            cfixed <- FALSE
        }
    } else {
        cfixed <- NULL;
    }

    out <- list(
        x = xval,
        y = yval,
        id = idval,
        ndigits = ndigits,
        signif = signif,
        pntsize = point.size,
        xlab = xlab,
        ylab = ylab,
        xjitter = x.jitter,
        yjitter = y.jitter,
        xord = x.ord,
        yord = y.ord,
        xlog = x.log,
        ylog = y.log,
        xgrid = xgrid,
        ygrid = ygrid,
        xstep = xstep,
        ystep = ystep,
        col = colval,
        cfixed = cfixed,
        anno = annot,
        annoLabels = annot.lab,
        height = height,
        width = width,
        json = json,
        type = "scatter",
        title = main,
        flag = flag,
        info = info,
        hide = hide,
        disableClick = "click" %in% disable,
        disableHover = "hover" %in% disable,
        disableZoom = "zoom" %in% disable
    )

    class(out) <- "jschart"

    out
}

constructScatterPlot <- function(chart, index, write.out) {
    command <- "glimma.storage.charts.push(glimma.chart.scatterChart()"

    height <- paste0(".height(", chart$height, ")")
    command <- paste0(command, height)

    width <- paste0(".width(", chart$width, ")")
    command <- paste0(command, width)

    if (not.null(chart$pntsize)) {
        size <- paste0(".size(function (d) { return ", chart$pntsize, "; })")
        command <- paste0(command, size)
    }

    x.func <- paste0(".x(function (d) { return d[", quotify(chart$x), "]; })")
    command <- paste0(command, x.func)

    x.lab <- paste0(".xlab(", quotify(chart$xlab), ")")
    command <- paste0(command, x.lab)

    x.jitter <- paste0(".xJitter(", chart$xjitter, ")")
    command <- paste0(command, x.jitter)

    if (chart$xord) {
        x.is.ord <- paste0(".xIsOrdinal()")
        command <- paste0(command, x.is.ord)
    }

    if (chart$xlog) {
        x.is.log <- paste0(".xIsLog()")
        command <- paste0(command, x.is.log)
    }

    y.func <- paste0(".y(function (d) { return d[", quotify(chart$y), "]; })")
    command <- paste0(command, y.func)

    y.lab <- paste0(".ylab(", quotify(chart$ylab), ")")
    command <- paste0(command, y.lab)

    y.jitter <- paste0(".yJitter(", chart$yjitter, ")")
    command <- paste0(command, y.jitter)

    if (chart$xgrid && chart$xstep) {
        command <- paste0(command, ".xGridOn(true)")
        command <- paste0(command, ".xGridStep(", chart$xstep, ")")
    }

    if (chart$ygrid && chart$ystep) {
        command <- paste0(command, ".yGridOn(true)")
        command <- paste0(command, ".yGridStep(", chart$ystep, ")")
    }

    if (chart$yord) {
        y.is.ord <- paste0(".yIsOrdinal()")
        command <- paste0(command, y.is.ord)
    }

    if (chart$ylog) {
        y.is.log <- paste0(".yIsLog()")
        command <- paste0(command, y.is.log)
    }

    if (not.null(chart$id)) {
        id <- paste0(".id(function (d) { return d[", quotify(chart$id), "]; })")
        command <- paste0(command, id)
    }

    anno <- paste0(".tooltip(glimma.storage.chartInfo[", index - 1, "].anno)")
    command <- paste0(command, anno)

    if (not.null(chart$annoLabels)) {
        annoLabels <- paste0(".tooltipLabels(glimma.storage.chartInfo[", index - 1, "].annoLabels)")
        command <- paste0(command, annoLabels)
    }

    main <- paste0(".title(glimma.storage.chartInfo[", index - 1, "].title)")
    command <- paste0(command, main)

    if (not.null(chart$ndigits)) {
        nformat <- paste0(".ndigits(", chart$ndigits, ")")
    } else {
        nformat <- paste0(".signif(", chart$signif, ")")
    }
    command <- paste0(command, nformat)

    if (not.null(chart$col)) {
        c.func <- paste0(".col(function(d) { return d[", quotify(chart$col), "]; })")
        command <- paste0(command, c.func)
        if (chart$cfixed) {
            cfix <- ".fixedCol(true)"
            command <- paste0(command, cfix)
        }
    }

    command <- paste0(command, ");\n")

    write.out(command)
}
