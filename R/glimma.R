#' Core glimma plot manager. Generates environment for glimma plots.
#' 
#' @param ... glimma plotting functions.
#' @param layout numeric vector specifying rows and columns of plots to create.
#' @return Generates interactive plots based on filling layout row by row from left to right.
#' @examples
#' 

glimma <- function(..., layout=d(1,1)) {
	# Convert variable arguments into list
	args <- list(...)

	cat("var glimma = window.glimma = [];\n", file="index.js")
	cat("var data = [];\n", file="data.js")	
	write.data <- writeMaker("data.js")

	actions <- data.frame(from=0, to=0, action="none")
	data.list <- list()

	for (i in 1:length(args)) {
		if (args[[i]]$type == "scatter") {
			scatterJS(args[[i]])
			plotCall(i)
			write.data(paste0("data.push(", args[[i]]$json, ");\n"))
		} else if (args[[i]]$type == "link") {
			actions <- cbind(actions, args[[i]]$link)
		}
	}

	cat("var interactions = ", file="interactions.js")
}

scatterJS <- function(chart) {
	write.out <- writeMaker("index.js")

	command <- "glimma.push(scatterChart().height(400)"

	x.func <- paste0(".x(function (d) { return d[", quotify(chart$x), "]; })")
	command <- paste0(command, x.func)

	y.func <- paste0(".y(function (d) { return d[", quotify(chart$y), "]; })")
	command <- paste0(command, y.func)

	if (!is.null(chart$col)) {
		c.func <- paste(".col(function(d) { return d[", quotify(chart$col), "]; })")
		command <- paste0(command, c.func)	
	}

	command <- paste0(command, ");\n")

	write.out(command)
}

plotCall <- function(index) {
	write.out <- writeMaker("index.js")

	command <- paste0("d3.select(\".glimma-plot.available\").datum(data[", index-1, "]).call(glimma[", index-1, "]);\n")

	write.out(command)
}