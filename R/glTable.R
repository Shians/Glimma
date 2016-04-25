#' Glimma Table
#' 
#' Create a table using the data from a chart.
#' 
#' @param target the index of the plot which receives the event and performs an action.
#' @param action the action to be performed at target plot using input information.
#' @param idval the column from which the autocomplete list will be populated.
#' @param flag indicates special flags for custom features.
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
