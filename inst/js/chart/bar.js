glimma.plot.barChart = function() {
	var margin = {top: 50, right: 20, bottom: 50, left: 60},
		width = 500,
		height = 400,
		signif = 6,
		ndigits = null,
		nValue = function(d) { return d.Id; },
		yValue = function(d) { return d.val; },
		sizeValue = function (d) { return 2; },
		titleValue = "",
		xLabel = "",
		yLabel = "",
		xScale = d3.scale.ordinal(),
		yScale = d3.scale.linear(),
		cScale = d3.scale.category10(),
		xAxis = d3.svg.axis().scale(xScale).orient("bottom").tickSize(6, 0),
		yAxis = d3.svg.axis().scale(yScale).orient("left").tickSize(6, 0);

	var dispatcher = d3.dispatch("hover", "leave", "click"),
		container,
		front,
		data,
		extent;

	function chart(selection) {
		var svg;

		occupyContainer();
		assignData();
		createDimensions();
		drawTitle();
		bindData();
		drawSkeleton();
		drawBars();
		drawAxis();
		bindDispatcher();

		function occupyContainer() {
			chart.container = container = selection;
			container.classed("available", false); // Mark plot window as occupied.
		}

		function assignData() {
			data = data || selection.data()[0]; // Grab data from plot window
		}

		function createDimensions() {
			// Scale initialisation
			xScale.domain(data.map(nValue)).rangeRoundBands([0, width - margin.left - margin.right], 0.1);
			yScale.domain([0, d3.max(data, yValue)]).range([height - margin.top - margin.bottom, 0]);
		}

		function drawTitle() {
			// Select title div if it exists, otherwise create it
			var titleDiv = selection.select(".title");
			if (titleDiv.node() === null) {
				titleDiv = selection.append("div")
										.attr("class", "title center-align")
										.style("width", width + "px")
										.html(titleValue);
			}
		}

		function bindData() {   
			// Bind data to SVG if it exists
			svg = selection.selectAll("svg").data([data]);
		}

		function drawSkeleton() {
			// Otherwise, create the skeletal chart.
			var gEnter = svg.enter().append("svg").append("g");
			gEnter.append("g").attr("class", "brush"); // brush
			gEnter.append("g").attr("class", "brush-cover"); // brush
			gEnter.append("g").attr("class", "x axis"); // x axis
			gEnter.append("g").attr("class", "x label center-align"); // x label
			gEnter.append("g").attr("class", "y axis"); // y axis
			gEnter.append("g").attr("class", "y label center-align"); // x label
			gEnter.append("g").attr("class", "bar-container"); // bar container
			front = gEnter.append("g").attr("class", "front"); // front layer
			if (container.select(".tooltip").node() === null) {
				container.append("div").attr("class", "tooltip padded rounded").style("opacity", 0); // tooltip
			}
			// Update the inner dimensions.
			var g = svg.select("g")
					.attr("transform", "translate(" + margin.left + "," + margin.top + ")");
			// Update the outer dimensions.
			svg.attr("width", width)
				.attr("height", height);
		}

		function drawBars() {
			var barContainer = svg.select(".bar-container")
									.selectAll("rect")
									.data(data, nValue);
		
			// Remove data bars that no longer exist
			barContainer.exit()
						.remove();

			// Add bars for new data
			barContainer.enter()
						.append("rect")
						.attr("class", "bar")
						.attr("x", function (d) { return xScale(nValue(d)); })
						.attr("y", function (d) { return yScale(yValue(d)); })
						.attr("height", function (d) { return height - margin.top - margin.bottom - yScale(yValue(d)); })
						.attr("width", xScale.rangeBand())
						.style("fill", "steelblue")
						.on("click", function (d) { dispatcher.click(d); })
						.on("mouseover", function (d) { dispatcher.hover(d); })
						.on("mouseout", function (d) { dispatcher.leave(d); });

			// Update positions
			barContainer.selectAll(".bar")
						.transition()
						.attr("y", function (d) { return yScale(yValue(d)); })
						.attr("height", function (d) { return height - margin.top - margin.bottom - yScale(yValue(d)); });
		}
		
		function drawAxis() {
			var tallTextOffset = 6;

			// Update the axes.
			svg.select(".x.axis")
					.attr("transform", "translate(0," + yScale.range()[0] + ")")
					.transition()
					.call(xAxis);
			var xLabSel = svg.select(".x.label");
			if (xLabSel.node().childElementCount  === 0) {
				xLabSel.append("text")
						.attr("class", "label-text")
						.attr("text-anchor", "middle")
						.attr("x", (width - margin.left) / 2)
						.attr("y", height - margin.top - tallTextOffset)
						.html(xLabel);
			}


			svg.select(".y.axis")
					.transition()
					.call(yAxis);
			var yLabSel = svg.select(".y.label");
			if (yLabSel.node().childElementCount  === 0) {
				yLabSel.attr("transform", "rotate(-90)")
						.append("text")
						.attr("class", "label-text")
						.attr("text-anchor", "middle")
						.attr("x", - (height - margin.top - margin.bottom) / 2)
						.attr("y", - (margin.left / 1.5))
						.html(yLabel);
			}
		}

		function bindDispatcher() {
			// Assign dispatcher events
			dispatcher.on("hover", function (d) { chart.highlight(d); });
			dispatcher.on("leave", function (d) { chart.lowlight(d); });
		}
	}
	
	//* Setters/getters *//
	chart.margin = function(_) {
		if (!arguments.length) return margin;
		margin = _;
		return chart;
	};

	chart.width = function(_) {
		if (!arguments.length) return width;
		width = _;
		return chart;
	};

	chart.height = function(_) {
		if (!arguments.length) return height;
		height = _;
		return chart;
	};

	chart.id = function(_) {
		if (!arguments.length) return nValue;
		nValue = _;
		return chart;
	};

	chart.xlab = function(_) {
		if (!arguments.length) return xLabel;
		xLabel = _;
		return chart;
	};

	chart.y = function(_) {
		if (!arguments.length) return yValue;
		yValue = _;
		return chart;
	};

	chart.ylab = function(_) {
		if (!arguments.length) return yLabel;
		yLabel = _;
		return chart;
	};

	chart.size = function(_) {
		if (!arguments.length) return sizeValue;
		sizeValue = _;
		return chart;
	};

	chart.data = function(_) {
		if (!arguments.length) return data;
		data = _;
		return chart;
	};

	chart.title = function(_) {
		if (!arguments.length) return titleValue;
		titleValue = _;
		return chart;
	};

	chart.signif = function(_) {
		if (!arguments.length) return signif;
		if (+_ % 1 === 0) {
			signif = _;
		}
		return chart;
	};

	chart.ndigits = function(_) {
		if (!arguments.length) return ndigits;
		if (+_ % 1 === 0) {
			ndigits = _;
		}
		return chart;
	};

	//* Internal Functions *//
	function _highlight(data) {
		_showTooltip(data);
	}

	function _lowlight() {
		container.select(".tooltip")
					.style("opacity", 0);
	}

	//* Helper Functions *//
	function _scaled_extent(data, key, factor) {
		factor = typeof factor !== "undefined" ? factor : 0.02;
		extent = d3.extent(data, key);
		range = extent[1] - extent[0];
		offset = range * factor;
		return [extent[0] - offset, extent[1] + offset];
	}

	function _showTooltip(data) {
		// TODO: Allow custom tooltip annotations.
		var tooltip = container.select(".tooltip");
		if (ndigits === null) {
			tooltip.html(glimma.math.signif(yValue(data), signif));
		} else {
			tooltip.html(glimma.math.round(yValue(data), ndigits));
		}

		var ttWidth = tooltip.node().offsetWidth,
			ttHeight = tooltip.node().offsetHeight,
			floatOffset = 3;

		var tooltipTop = yScale(yValue(data)) + container.select("svg").node().offsetTop;
		tooltipTop += margin.top - ttHeight - floatOffset;

		var tooltipLeft = xScale(nValue(data)) + container.select("svg").node().offsetLeft;
		tooltipLeft += margin.left + (xScale.rangeBand() - ttWidth) / 2;

		// TODO, Top positioning might be unreliable.
		tooltip
		.style("top", tooltipTop + "px")
		.style("left", tooltipLeft + "px")
		.style("opacity", 1);
	}

	function _hideTooltip() {
		container.select(".tooltip")
					.style("opacity", 0);
	}

	//* Interactions *//
	chart.highlight = function (d) {
		_highlight(d);
	};

	chart.lowlight = function (d) {
		_lowlight();
	};

	chart.update = function () {
		container.call(chart);
	};

	// This allows other objects to 'listen' to events dispatched by the _table object.
	d3.rebind(chart, dispatcher, "on");
	
	return chart;
};
