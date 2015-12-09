#' Create an interactive scatter plot
#' 
#' @param x A data.frame containing data to plot
#' @return 
#' @examples
#' 
interactiveScatter <- function(x, ...) {
	UseMethod("interactiveScatter")
}

interactiveScatter.default <- function(x, xval="x", yval="y", colval=NULL, annot=c("x", "y"), path=getwd()) {
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

	##
	# Function body
	out <- list()

	# Make json out of data
	x <- data.frame(x)
	json <- makeDFJson(x)

	if (char(path, -1) != "/") {	
		path <- paste0(path, "/")
	}

	file.path <- pathMaker(path)

	out$x <- xval
	out$y <- yval
	out$col <- colval
	out$anno <- annot
	out$json <- json
	out$type <- "scatter"

	class(out) <- "jschart"

	out	
}
