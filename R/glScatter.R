#' Create an interactive scatter plot
#' 
#' @param x A data.frame containing data to plot
#' @return 
#' @examples
#' 
glScatter <- function(x, ...) {
	UseMethod("glScatter")
}

glScatter.default <- function(x, xval="x", yval="y", 
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
