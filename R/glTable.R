#' Glimma Table
#' 
#' Create a table using the data from a chart.
#' 
#' @param target the index of the plot from which data is drawn.
#' @param columns the columns of data to plot.
#' 
#' @return a input object containing the input field information.

glTable <- function(target, columns) {
    out <- list()
    out$input <- data.frame(target=target)
    out$columns <- columns
    out$type <- "data.table"
    class(out) <- "jstable"

    return(out)
}
