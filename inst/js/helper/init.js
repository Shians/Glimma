// Cycle through constructed plots
if (d3.select(".glimma-plot.available").node()) {
	for (var i=0; i<glimma.chartInfo.length; i++) {
		var chart = glimma.chartInfo[i];
		d3.select(".glimma-plot.available").datum(glimma.chartData[i]).call(glimma.charts[i]);
	}
}
