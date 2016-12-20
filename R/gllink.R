#' Plot linkages
#'
#' Helper function for writing the link properties in interactive Glimma plots
#'
#' @param from the index of the plot from which the event is dispatched.
#' @param to the index of the plot which receives the event and performs an action.
#' @param src the action that is performed in the "from" plot.
#' @param dest the action that is performed in the "to" plot.
#' @param flag indicates special links for particular chart types.
#' @param both creates symmetric links whereby the "dest" action in "to" also triggers the "src" action in "from".
#' @param info additional info for creating the link.
#'
#' @return a link object containing the plot linking information.
#'
#' @examples
#' data(iris)
#' data <- data.frame(Name=paste("Flower", 1:nrow(iris), sep="-"), iris)
#' \donttest{
#' plot1 <- glScatter(data, xval="Sepal.Length", yval="Sepal.Width", colval="Species")
#' plot2 <- glScatter(data, xval="Species", yval="Petal.Length", colval="Species")
#' link1 <- gllink(1, 2, src="hover", dest="hover", both=TRUE)
#' glimma(plot1, plot2, link1, layout=c(1,2))
#' }

gllink <- function(from, to, src="none", dest="none", flag="none", both=FALSE, info="none") {
    out <- list()

    if (src != "none" && dest == "none") {
        stop("src cannot be defined while dest is 'none'")
    }

    if (src == "none" && dest != "none") {
        stop("dest cannot be defined while src is 'none'")
    }

    if (src == "none" && dest == "none" && flag == "none") {
        stop("'src', 'dest' and 'flag' cannot simultaneously be 'none'")
    }

    out$link <- data.frame(from=from, to=to, src=src, dest=dest, flag=flag, info=info)
    if (both) {
        out$link <- rbind(out$link, data.frame(from=to, to=from, src=dest, dest=src, flag=flag, info=info))
    }

    out$type <- "link"

    class(out) <- "jslink"
    return(out)
}
