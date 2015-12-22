// Cycle through constructed plots
for (var i=0; i<glimma.chartInfo.length; i++) {
	var chart = glimma.chartInfo[i];
	d3.select(".glimma-plot.available").datum(glimma.data[i]).call(glimma.charts[i]);
}