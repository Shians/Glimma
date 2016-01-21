#' Create an interactive bar plot object
#' 
#' @param x A data.frame containing data to plot
#' @return a "chart" object containing information used by glimma to create plots
#' @export
#' @examples
#' 

glMDSPlot <- function(x, ...) {
  UseMethod("glMDSPlot")
}

glMDSPlot.default <- function(x, k=8) {
	# Make json out of data
	x <- data.frame(x)
	d <- dist(x)
	fit <- cmdscale(d, eig=TRUE, k=k)

	dims <- fit$points
	colnames(dims) <- paste0("dim", 1:ncol(dims))

	eigs <- data.frame(name=1:8, val=fit$eig)

	scatter <- glScatter(dims, xval="dim1", yval="dim2")
	# bar <- glBar()
}
