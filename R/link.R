#' Helper function for writing the link properties in interactive Glimma plots
#' 
#' @param from the index of the plot from which the event is dispatched.
#' @param to the index of the plot which receives the event and performs an action.
#' @param action the action that is performed in the "to" plot.
#' @return a link object containing the plot linking information.
#' @examples 
#' 

link <- function(from, to, action, both=FALSE) {
	out <- list()
	out$link <- data.frame(from=from, to=to, action=action)
	if (both) {
		out$link <- rbind(out$link, data.frame(from=to, to=from, action=action))
	}
	out$type <- "link"
	return(out)
}