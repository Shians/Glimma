for (var i=0; i<glimma.chartInfo.length; i++) {
	var chart = glimma.chartInfo[i];

	// Cycle through constructed plots
	if (chart.type == "scatter") {
		d3.select(".glimma-plot.available").datum(glimma.data[i]).call(glimma.charts[i]);
	} else {

	}
}