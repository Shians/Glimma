#' Helper function for writing the link properties in interactive Glimma plots
#' 
#' @param from the index of the plot from which the event is dispatched.
#' @param to the index of the plot which receives the event and performs an action.
#' @param src the action that is performed in the "from" plot.
#' @param dest the action that is performed in the "to" plot.
#' @param flag indicates special links for particular chart types.
#' @param both creates symmetric links whereby the "dest" action in "to" also triggers the "src" action in "from".
#' @return a link object containing the plot linking information.
#' @examples 
#' 

link <- function(from, to, src, dest, flag=NULL, both=FALSE) {
	out <- list()
	out$link <- data.frame(from=from, to=to, src=src, dest=dest)
	if (both) {
		out$link <- rbind(out$link, data.frame(from=to, to=from, src=dest, dest=src))
	}
	out$type <- "link"
	out$flag <- flag
	return(out)
}