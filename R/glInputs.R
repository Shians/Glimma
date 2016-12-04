#' Glimma Autocomplete Input
#'
#' Create an autocompleted input field.
#'
#' @param target the index of the plot which receives the event and performs an action.
#' @param action the action to be performed at target plot using input information.
#' @param idval the column from which the autocomplete list will be populated.
#' @param flag indicates special flags for custom features.
#'
#' @return a input object containing the input field information.

glAutoinput <- function(target, action, idval="none", flag="none") {
    out <- list()
    out$input <- data.frame(target=target, action=action, idval=idval, flag=flag)
    out$type <- "autocomplete"
    class(out) <- "jsinput"

    return(out)
}
