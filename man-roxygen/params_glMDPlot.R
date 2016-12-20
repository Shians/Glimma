#' @param counts the matrix of expression values, with samples in columns.
#' @param anno the data.frame containing gene annotations.
#' @param groups the factor containing experimental groups of the samples.
#' @param samples the names of the samples.
#' @param status vector giving the control status of data point, of same length
#'   as the number of rows of object. If NULL, then all points are plotted in
#'   the default colour.
#' @param transform TRUE if counts cpm transformed.
#' @param side.main the column containing mains for right plot.
#' @param side.xlab label for x axis on right plot.
#' @param side.ylab label for y axis on right plot.
#' @param side.log TRUE to plot expression on the right plot on log scale.
#' @param side.gridstep intervals along which to place grid lines on y axis.
#'   Currently only available for linear scale.
#' @param jitter the amount of jitter to apply to the samples in the expressions
#'   plot.
#' @param display.columns character vector containing names of columns to
#'   display in mouseover tooltips and table.
#' @param cols vector of strings denoting colours corresponding to control
#'   status -1, 0 and 1. (may be R named colours or Hex values)
#' @param sample.cols vector of strings denoting colours for each sample point
#'   on the expression plot.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param launch TRUE to launch plot after call.
