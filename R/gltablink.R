#' Plot linkages
#'
#' Helper function for writing the link properties in interactive Glimma plots
#'
#' @param from the index of the source table.
#' @param to the index of the plot which receives the event and performs an action.
#' @param action the action that is performed in the plot.
#' @param info additional info for creating the link.
#'
#' @return a link object containing the plot linking information.


gltablink <- function(from, to, action="none", info="none") {
    out <- list()

    checkThat(from, isNumeric)
    checkThat(to, isNumeric)
    checkThat(action, isCharacter)
    checkThat(info, isCharacter)

    out <- gllink(from=from, to=to,
                    src="click", dest=action, flag="tablink", info=info)

    out$type <- "tablink"

    class(out) <- "jslink"
    return(out)
}
