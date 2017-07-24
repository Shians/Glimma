#' Glimma MD Plot
#'
#' Create an interactive bar plot object.
#'
#' @author Shian Su
#'
#' @param x the data.frame containing data to plot.
#' @param ... additional arguments depending on input object type.
#'
#' @return A chart object containing the information to create an interactive bar plot.
#'
#' @seealso \code{\link{glBar.default}}
#'
#' @examples
#' data(mtcars)
#' counts <- table(mtcars$gear)
#' data <- data.frame(nGears=as.numeric(names(counts)), Count=as.numeric(counts))
#' \donttest{
#' plot1 <- glBar(data, "Count", "nGears", ylab="Number of Gears")
#' glimma(plot1, layout=c(1,1), launch=TRUE)
#' }

glBar <- function(x, ...) {
    UseMethod("glBar")
}

#' Glimma MD Plot
#'
#' Default method for interactive bar plot.
#'
#' @author Shian Su
#'
#' @param x the data.frame containing data to plot.
#' @param yval the column name for the x-axis values.
#' @param names.arg the column name for the label on each bar.
#' @param ndigits the number of digits after the decimal to round to in the tooltip (overrides signif).
#' @param signif the number of significant figures to display in the tooltip.
#' @param xlab the label on the x-axis.
#' @param ylab the label on the y-axis.
#' @param main the title for the plot.
#' @param height the height of the plot (in pixels).
#' @param width the width of the plot (in pixels).
#' @param colval the colours for each data point.
#' @param annot the columns to display in the tooltip.
#' @param flag the special flag to indicate special plot.
#' @param info additional information for plotting.
#' @param ... additional arguments.
#'
#' @return A chart object containing the information to create an interactive bar plot.
#'
#' @method glBar default
#'
#' @examples
#' data(mtcars)
#' counts <- table(mtcars$gear)
#' data <- data.frame(nGears=as.numeric(names(counts)), Count=as.numeric(counts))
#' \donttest{
#' plot1 <- glBar(data, "Count", "nGears", ylab="Number of Gears")
#' glimma(plot1, layout=c(1,1), launch=TRUE)
#' }

glBar.default <- function(x, yval, names.arg=rownames(x),
                            ndigits=NULL, signif=6,
                            xlab=NULL, ylab=yval, main=NULL,
                            height=400, width=500,
                            colval=NULL, annot=yval,
                            flag=NULL, info=NULL, ...) {
    ##
    # Input checking
    if (is.na(match(yval, names(x)))) {
        stop(paste(yval, "does not correspond to a column"))
    }

    if (is.na(match(names.arg, names(x)))) {
        stop(paste(names.arg, "does not correspond to a column"))
    }

    if (not.null(colval)) {
        if (is.na(match(colval, names(x)))) {
            stop(paste(colval, "does not correspond to a column"))
        }
    }
    #
    ##

    # Make json out of data
    x <- data.frame(x)
    json <- makeJson(x, dataframe = "columns")

    out <- list(
                names = names.arg,
                y = yval,
                ndigits = ndigits,
                signif = signif,
                xlab = xlab,
                ylab = ylab,
                col = colval,
                anno = annot,
                height = height,
                width = width,
                json = json,
                type = "bar",
                title = main,
                flag = flag,
                info = info
            )

    class(out) <- "jschart"

    out
}

# Helper for writing js commands to draw plot
constructBarPlot <- function(chart, index, write.out) {
    command <- "glimma.storage.charts.push(glimma.chart.barChart()"

    height <- paste0(".height(", chart$height, ")")
    command <- paste0(command, height)

    width <- paste0(".width(", chart$width, ")")
    command <- paste0(command, width)

    x.func <- paste0(".id(function (d) { return d[", quotify(chart$names), "]; })")
    command <- paste0(command, x.func)

    x.lab <- paste0(".xlab(", quotify(chart$xlab), ")")
    command <- paste0(command, x.lab)

    y.func <- paste0(".y(function (d) { return d[", quotify(chart$y), "]; })")
    command <- paste0(command, y.func)

    y.lab <- paste0(".ylab(", quotify(chart$ylab), ")")
    command <- paste0(command, y.lab)

    main <- paste0(".title(glimma.storage.chartInfo[", index - 1, "].title)")
    command <- paste0(command, main)

    if (not.null(chart$ndigits)) {
        nformat <- paste0(".ndigits(", chart$ndigits, ")")
    } else {
        nformat <- paste0(".signif(", chart$signif, ")")
    }
    command <- paste0(command, nformat)

    command <- paste0(command, ");\n")

    write.out(command)
}
