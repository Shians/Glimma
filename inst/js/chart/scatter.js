function scatterChart() {
    var margin = {top: 20, right: 20, bottom: 40, left: 40};
    var width = 500;
    var height = 500;
    var xValue = function(d) { return d.x; };
    var yValue = function(d) { return d.y; };
    var sizeValue = function (d) { return 2; };
    var cValue = function (d) { return "black"; };
    var tooltip = ["x", "y"];
    var xScale = d3.scale.linear();
    var yScale = d3.scale.linear();
    var cScale = d3.scale.category10();
    var xAxis = d3.svg.axis().scale(xScale).orient("bottom").tickSize(6, 0);
    var yAxis = d3.svg.axis().scale(yScale).orient("left").tickSize(6, 0);

    var dispatcher = d3.dispatch("hover", "leave");
    var container;
    var front;
    var data;
    var extent;

    function chart(selection) {
        chart.container = container = selection;
        container.classed("available", false); // Mark plot window as occupied.

        data = data || selection.data()[0]; // Grab data from plot window

        extent = extent || {"x": _scaled_extent(data, xValue), "y": _scaled_extent(data, yValue)};

        // Scale initialisation
        xScale.domain(extent.x).range([0, width - margin.left - margin.right]);
        yScale.domain(extent.y).range([height - margin.top - margin.bottom, 0]);
        cScale.domain(data.map(function (d) { return cValue(d); }).unique()); //TODO: Allow fill with cValue without mapping

        // Create brush object
        var brush = d3.svg.brush().x(xScale).y(yScale).on("brushend", _brushend);

        // Bind data to SVG if it exists
        var svg = selection.selectAll("svg").data([data]);

        // Otherwise, create the skeletal chart.
        var gEnter = svg.enter().append("svg").append("g");
        gEnter.append("g").attr("class", "brush"); // brush
        gEnter.append("g").attr("class", "x axis"); // x axis
        gEnter.append("g").attr("class", "y axis"); // y axis
        gEnter.append("g").attr("class", "circle_container"); // circle container
        front = gEnter.append("g").attr("class", "front"); // front layer
        container.select(".tooltip").node() || 
        container.append("div").attr("class", "tooltip").style("opacity", 0); // tooltip

        svg.select(".brush").call(brush);

        // Update the outer dimensions.
        svg.attr("width", width)
            .attr("height", height);

        // Update the inner dimensions.
        var g = svg.select("g")
                .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        var cirContainer = svg.select(".circle_container")
                                .selectAll("circle")
                                .data(data, function(d) { return [xValue(d), yValue(d)]; })
        
                
        cirContainer.exit()
                    .remove();

        // Create container for circles.
        cirContainer.enter()
                    .append("circle")
                    .attr("class", "point")
                    .attr("cx", function (d) { return xScale(xValue(d)); })
                    .attr("cy", function (d) { return yScale(yValue(d)); })
                    .attr("r", function (d) { return sizeValue(d); })
                    .style("fill", function (d) { return cScale(cValue(d)); })
                    .on('mouseover', function (d) { dispatcher.hover(d); })
                    .on('mouseout', function (d) { dispatcher.leave(d); });

        cirContainer.transition()
                    .attr("cx", function (d) { return xScale(xValue(d)); })
                    .attr("cy", function (d) { return yScale(yValue(d)); });

        // Update the axes.
        svg.select(".x.axis")
                .attr("transform", "translate(0," + yScale.range()[0] + ")")
                .transition()
                .call(xAxis);
        svg.select(".y.axis")
                .transition()
                .call(yAxis);

        // Assign dispatcher events
        dispatcher.on("hover", function (d) { chart.hover(d); });
        dispatcher.on("leave", function (d) { chart.leave(d); });

        // Brush function
        function _brushend() {
            var extent = brush.extent();
            svg.select(".brush").call(brush.clear());
            _rescale(extent);
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

    chart.x = function(_) {
        if (!arguments.length) return xValue;
        xValue = _;
        return chart;
    };

    chart.y = function(_) {
        if (!arguments.length) return yValue;
        yValue = _;
        return chart;
    };

    chart.size = function(_) {
        if (!arguments.length) return sizeValue;
        sizeValue = _;
        return chart;
    };

    chart.col = function(_) {
        if (!arguments.length) return cValue;
        cValue = _;
        return chart;
    };

    chart.tooltip = function(_) {
        if (!arguments.length) return tooltip;
        tooltip = _;
        return chart;
    }

    chart.data = function(_) {
        if (!arguments.length) return data;
        data = _;
        return chart;
    }

    chart.extent = function(_) {
        if (!arguments.length) return extent;
        extent = _;
        return chart;
    }

    //* Helper Functions *//
    function _scaled_extent(data, key, factor) {
        factor = typeof factor !== "undefined" ? factor : 0.02;
        extent = d3.extent(data, key);
        range = extent[1] - extent[0];
        offset = range * factor;
        return [extent[0] - offset, extent[1] + offset]
    }

    function _showTooltip(data) {

        container.select(".tooltip")
                    .select("table")
                    .remove();

        var table = container.select(".tooltip")
                                .append("table");

        for (var i=0; i<tooltip.length; i++) {
            var row = table.append("tr");
            row.append("td").attr("class", "right-align tooltip-cell").html(tooltip[i] +":");
            row.append("td").attr("class", "left-align tooltip-cell").html(data[tooltip[i]]);
        }

        tooltipLeft = xScale(xValue(data));
        tooltipLeft += margin.left + margin.right;
        // tooltipLeft -= 3 + container.select(".tooltip").node().offsetWidth;

        tooltipTop = yScale(yValue(data));
        tooltipTop += margin.top;
        tooltipTop -= 3 + container.select(".tooltip").node().offsetHeight;
        tooltipTop = tooltipTop < 0 ? 0 : tooltipTop;
                     

        container.select(".tooltip")
                    .style("opacity", 1)
                    .style("left", tooltipLeft + "px")
                    .style("top", tooltipTop + "px");
    }

    function _rescale(extent) {
        var newData = container.data()[0].filter(function (d) { return _within(d, extent); })
        chart.data(newData);
        chart.extent({"x": [extent[0][0], extent[1][0]], "y": [extent[0][1], extent[1][1]]});
        container.call(chart);
    }

    function _resetScale() {
        chart.data(container.data()[0]);
        extent = null;
        container.call(chart);   
    }

    function _within(point, extent) {
        var x = xValue(point);
        var y = yValue(point);

        return (x >= extent[0][0] && 
                x <= extent[1][0] &&
                y >= extent[0][1] &&
                y <= extent[1][1])
    }

    //* Interactions *//
    chart.hover = function(data) {
        var c = front.select("circle")
        if (c[0][0] === null) {
            c = front.append("circle")
        }
        c.attr("cx", xScale(xValue(data)))
            .attr("cy", yScale(yValue(data)))
            .attr("r", sizeValue(data) + 2)
            .style("opacity", 1)
            .style("stroke", "white")
            .style("fill", cScale(cValue(data)));

        _showTooltip(data);
    };

    chart.leave = function(data) {
        front.selectAll("circle")
                .style("opacity", 0);

        container.select(".tooltip")
                    .style("opacity", 0);
    };

    chart.rescale = function(extent) {
        _rescale(extent);
    };

    chart.update = function() {
        container.call(chart);
    };

    // This allows other objects to 'listen' to events dispatched by the _table object.
    d3.rebind(chart, dispatcher, "on");
    
    return chart;
}