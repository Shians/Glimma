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
#' @param x.jitter the amount of jittering to add to values along the x axis.
#' @param y.jitter the amount of jittering to add to values along the y axis.
#' @param idval the column name for unique identifiers.
#' @param ndigits the number of digits after the decimal to round to in the tooltip (overrides signif).
#' @param signif the number of significant figures to display in the tooltip.
#' @param log a character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if both axes are to be logarithmic.
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

glScatter.default <- function(x, xval="x", yval="y", idval=NULL,
                                x.jitter = 0, y.jitter = 0,
                                ndigits=NULL, signif=6, log="",
                                xlab=xval, ylab=yval, main=NULL,
                                height=400, width=500,
                                colval=NULL, annot=c(xval, yval), annot.lab=NULL,
                                flag=NULL, info=NULL, hide=FALSE, ...) {
    ##
    # Input checking
    assertClass(xval, "character")
    assertClass(yval, "character")
    assertClass(annot, "character")

    if (is.na(match(xval, names(x)))) {
        stop(paste(xval, "does not correspond to a column"))
    }

    if (is.na(match(yval, names(x)))) {
        stop(paste(yval, "does not correspond to a column"))
    }

    if (!is.null(colval)) {
        if (is.na(match(colval, names(x)))) {
            stop(paste(colval, "does not correspond to a column"))
        }
    }

    if (!is.null(idval)) {
        if (is.na(match(idval, names(x)))) {
            stop(paste(idval, "does not correspond to a column"))
        }
    }
    # TODO: Ensure uniqueness of identifiers
    # TODO: Generate default identifiers if not present

    if (any(is.na(match(annot, names(x))))) {
        stop(paste("not all values in annot correspond to a column"))
    }
    #
    ##

    # Normalise input
    x <- data.frame(x)

    # TODO: Consider using rjson package?
    # Make json out of data
    json <- makeDFJson(x)

    x.ord <- is.factor(x[[xval]])
    y.ord <- is.factor(x[[yval]])
    x.log <- "x" %in% unlist(strsplit(log, NULL))
    y.log <- "y" %in% unlist(strsplit(log, NULL))

    if (!is.null(colval)) {
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
                xlab = xlab,
                xjitter = x.jitter,
                ylab = ylab,
                yjitter = y.jitter,
                xord = x.ord,
                yord = y.ord,
                xlog = x.log,
                ylog = y.log,
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
                hide = hide
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

    if (chart$yord) {
        y.is.ord <- paste0(".yIsOrdinal()")
        command <- paste0(command, y.is.ord)
    }

    if (chart$ylog) {
        y.is.log <- paste0(".yIsLog()")
        command <- paste0(command, y.is.log)
    }

    if (!is.null(chart$id)) {
        id <- paste0(".id(function (d) { return d[", quotify(chart$id), "]; })")
        command <- paste0(command, id)
    }

    anno <- paste0(".tooltip(glimma.storage.chartInfo[", index - 1, "].anno)")
    command <- paste0(command, anno)

    if (!is.null(chart$annoLabels)) {
        annoLabels <- paste0(".tooltipLabels(glimma.storage.chartInfo[", index - 1, "].annoLabels)")
        command <- paste0(command, annoLabels)
    }

    main <- paste0(".title(glimma.storage.chartInfo[", index - 1, "].title)")
    command <- paste0(command, main)

    if (!is.null(chart$ndigits)) {
        nformat <- paste0(".ndigits(", chart$ndigits, ")")
    } else {
        nformat <- paste0(".signif(", chart$signif, ")")
    }
    command <- paste0(command, nformat)

    if (!is.null(chart$col)) {
        c.func <- paste0(".col(function(d) { return d[", quotify(chart$col), "]; })")
        command <- paste0(command, c.func)
        if (chart$cfixed) {
            cfix <- ".fixedCol(true)"
            command <- paste0(command, cfix)
        }
    }

    if (!is.null(chart$info)) {
        if (!is.null(chart$info$search.by)) {
            search <- paste0(".searchValue(function(d) { return d[", quotify(chart$info$search.by), "]; })")
            command <- paste0(command, search)
        }
    }

    command <- paste0(command, ");\n")

    write.out(command)
}
