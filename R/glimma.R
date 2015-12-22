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
			constructScatterPlot(args[[i]], i)
		} else if (args[[i]]$type == "bar") {
			# Write json data
			write.data(paste0("glimma.data.push(", args[[i]]$json, ");\n"))
			
			# Write plot information
			args[[i]]$json <- NULL
			chartInfo <- makeChartJson(args[[i]])
			write.data(paste0("glimma.chartInfo.push(", chartInfo, ");\n"))

			# Write plot call
			constructBarPlot(args[[i]], i)
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