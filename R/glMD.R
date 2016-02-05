#' Darw an interactive MD plot
#' 
#' @param x the data.frame containing data to plot.
#' @return writes an interactive MDS plot
#' @export
#' @examples
#' 

glMDSPlot <- function(x, ...) {
  UseMethod("glMDSPlot")
}

#' @export
glMDSPlot.default <- function(x, ...) {

}

#' @export
glMDPlot.DGEList <- function (x, ...) {
	
}