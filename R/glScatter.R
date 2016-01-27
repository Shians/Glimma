#' Create an interactive scatter plot object
#' 
#' @param x the data.frame containing data to plot.
#' @param xval the column name for the x-axis values.
#' @param yval the column name for the y-axis values.
#' @param id the column name for unique identifiers.
#' @param ndigits the number of digits after the decimal to round to in the tooltip (overrides signif).
#' @param signif the number of significant figures to display in the tooltip.
#' @param xlab the label on the x-axis.
#' @param ylab the label on the y-axis.
#' @param main the title for the plot.
#' @param height the height of the plot (in pixels).
#' @param width the width of the plot (in pixels).
#' @param colval the colours for each data point.
#' @param annot the columns to display in the tooltip.
#' @param ... additional arguments depending on input object type.
#' @return A chart object containing the information to create an interactive scatter plot.
#' @export
#' @examples
#' data(iris)
#' plot1 <- glScatter(iris, xval="Sepal.Length", yval="Sepal.Width", colval="Species")
#' glimma(plot1, c(1,1))
#'

glScatter <- function(x, ...) {
	UseMethod("glScatter")
}

#' @export

glScatter.default <- function(x, xval="x", yval="y", id=NULL,
								ndigits=NULL, signif=6,
								xlab=xval, ylab=yval, main=NULL,
								height=400, width=500,
								colval=NULL, annot=c(xval, yval)) {
	##
	# Input checking
	if (!is.character(xval)) {
		stopType("character", "xval")
	}
	if (!is.character(yval)) {
		stopType("character", "yval")	
	}
	if (!is.character(annot)) {
		stopType("character", "annot")	
	}

	if (is.na(match(xval, names(x)))) {
		stop(paste(xval, "does not correspond to a column"))
	}

	if (is.na(match(yval, names(x)))) {
		stop(paste(yval, "does not correspond to a column"))
	}

	if (any(is.na(match(annot, names(x))))) {
		stop(paste("not all values in annot correspond to a column"))
	}
	#
	##

	# Normalise input
	x <- data.frame(x)

	# Make json out of data
	json <- makeDFJson(x)

	out <- list(
				x = xval,
				y = yval,
				id = id,
				ndigits = ndigits,
				signif = signif,
				xlab = xlab,
				ylab = ylab,
				col = colval,
				anno = annot,
				height = height,
				width = width,
				json = json,
				type = "scatter",
				title = main
			)

	class(out) <- "jschart"

	out
}

constructScatterPlot <- function(chart, index, write.out) {
	command <- "glimma.charts.push(glimma.chart.scatterChart()"

	height <- paste0(".height(", chart$height, ")")
	command <- paste0(command, height)

	width <- paste0(".width(", chart$width, ")")
	command <- paste0(command, width)

	x.func <- paste0(".x(function (d) { return d[", quotify(chart$x), "]; })")
	command <- paste0(command, x.func)

	x.lab <- paste0(".xlab(", quotify(chart$xlab), ")")
	command <- paste0(command, x.lab)

	y.func <- paste0(".y(function (d) { return d[", quotify(chart$y), "]; })")
	command <- paste0(command, y.func)

	y.lab <- paste0(".ylab(", quotify(chart$ylab), ")")
	command <- paste0(command, y.lab)

	if (!is.null(chart$id)) {
		id <- paste0(".id(", quotify(chart$id), ")")
		command <- paste0(command, id)
	}

	anno <- paste0(".tooltip(glimma.chartInfo[", index - 1, "].anno)")
	command <- paste0(command, anno)

	main <- paste0(".title(glimma.chartInfo[", index - 1, "].title)")
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
	}

	command <- paste0(command, ");\n")

	write.out(command)
}