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

	cat("var glimma = window.glimma = [];\n",
		"glimma.data = [];\n",
		"glimma.chartInfo = [];\n",
		"glimma.charts = [];\n",
		"glimma.linkage = [];\n",
		file="data.js", sep="")

	cat("var glimma = window.glimma = [];\n", file="index.js");

	write.data <- writeMaker("data.js")

	actions <- data.frame(from=0, to=0, action="none") # Dummy row
	data.list <- list()

	for (i in 1:length(args)) {
		if (args[[i]]$type == "link") {
			actions <- rbind(actions, args[[i]]$link)
		} else if (args[[i]]$type == "scatter") {
			# Write json data
			write.data(paste0("glimma.data.push(", args[[i]]$json, ");\n"))
			
			# Write plot information
			args[[i]]$json <- NULL
			chartInfo <- makeChartJson(args[[i]])
			write.data(paste0("glimma.chartInfo.push(", chartInfo, ");\n"))

			# Write plot call
			scatterJS(args[[i]], i)
		}
	}

	# Write linkage
	if (nrow(actions) > 1) {
		actions.js <- makeDFJson(actions[-1, ])
		write.data(paste0("glimma.linkage = ", actions.js, ";"))
	} else {
		write.data("glimma.linkage = [];")
	}
}

scatterJS <- function(chart, index) {
	write.out <- writeMaker("data.js")

	command <- "glimma.charts.push(scatterChart().height(400)"

	x.func <- paste0(".x(function (d) { return d[", quotify(chart$x), "]; })")
	command <- paste0(command, x.func)

	y.func <- paste0(".y(function (d) { return d[", quotify(chart$y), "]; })")
	command <- paste0(command, y.func)

	anno <- paste0(".tooltip(glimma.chartInfo[", index - 1, "].anno)")
	command <- paste0(command, anno)

	if (!is.null(chart$col)) {
		c.func <- paste(".col(function(d) { return d[", quotify(chart$col), "]; })")
		command <- paste0(command, c.func)	
	}

	command <- paste0(command, ");\n")

	write.out(command)
}

plotCall <- function(index) {
	write.out <- writeMaker("index.js")

	command <- paste0("d3.select(\".glimma-plot.available\")", 
							".datum(glimma.data[", index-1, "])",
							".call(glimma.charts[", index-1, "]);\n")

	write.out(command)
}