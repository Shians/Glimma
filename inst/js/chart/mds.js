function mdsChart() {
	var chart = scatterChart();

	chart._swapDim <- function (dim1, dim2) {
		chart.x(function (d) { return d[["dim" + dim1]]; })
    	chart.y(function (d) { return d[["dim" + dim2]]; })

    	chart.xlab("Dimension " + dim1);
    	chart.ylab("Dimension " + dim2);
    	chart.update();
	}
	
	return chart
}