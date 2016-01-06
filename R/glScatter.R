#' Create an interactive scatter plot object
#' 
#' @param x A data.frame containing data to plot
#' @return 
#' @examples
#' 
glScatter <- function(x, ...) {
	UseMethod("glScatter")
}

glScatter.default <- function(x, xval="x", yval="y", 
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

	# Make json out of data
	x <- data.frame(x)
	json <- makeDFJson(x)

	out <- list(
				x = xval,
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
				type = "scatter",
				title = main
			)

	class(out) <- "jschart"

	out
}

constructScatterPlot <- function(chart, index) {
	write.out <- writeMaker("data.js")

	command <- "glimma.charts.push(glimma.plot.scatterChart()"

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