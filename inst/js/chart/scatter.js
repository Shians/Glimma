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

    function chart(selection) {
        container = selection;
        container.classed("available", false);

        var data = selection.data()[0];

        // Update the x-scale.
        xScale.domain(_scaled_extent(data, xValue))
                .range([0, width - margin.left - margin.right]);

        // Update the y-scale.
        yScale.domain(_scaled_extent(data, yValue))
                .range([height - margin.top - margin.bottom, 0]);

        // Update the color-scale.
        cScale.domain(data.map(function (d) { return cValue(d); }).unique()); //TODO: Allow fill with cValue without mapping

        // Otherwise, create the skeletal chart.
        var gEnter = selection.append("svg").append("g");
        gEnter.append("g").attr("class", "x axis");
        gEnter.append("g").attr("class", "y axis");
        gEnter.append("g").attr("class", "circle_container");
        front = gEnter.append("g").attr("class", "front");

        container.append("div")
                    .attr("class", "tooltip")
                    .style("opacity", 0);

        var svg = selection.select("svg");

        var zoom = d3.behavior.zoom().scaleExtent([1,8]).x(xScale).y(yScale).on("zoom", zoom);

        // Update the outer dimensions.
        svg .attr("width", width)
                .attr("height", height);

        // Update the inner dimensions.
        var g = svg.select("g")
                .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        g.call(zoom);

        // Update the area path.
        svg.select(".circle_container")
                .selectAll("circle")
                .data(data)
                .enter()
                .append("circle")
                .attr("cx", function (d) { return xScale(xValue(d)); })
                .attr("cy", function (d) { return yScale(yValue(d)); })
                .attr("r", function (d) { return sizeValue(d); })
                .attr("transform", transform)
                .style("fill", function (d) { return cScale(cValue(d)); })
                .on('mouseover', function (d) { dispatcher.hover(d); })
                .on('mouseout', function (d) { dispatcher.leave(d); });

        // Update the x-axis.
        g.select(".x.axis")
                .attr("transform", "translate(0," + yScale.range()[0] + ")")
                .call(xAxis);

        g.select(".y.axis")
                .call(yAxis);

        dispatcher.on("hover", function (d) { chart.hover(d); });
        dispatcher.on("leave", function (d) { chart.leave(d); });
    }

    //* Setters/getters *//

    function X(d) {
        return xScale(d[0]);
    }

    function Y(d) {
        return yScale(d[1]);
    }

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

    //* Helper Functions *//
    function _scaled_extent(data, key, factor) {
        factor = typeof factor !== "undefined" ? factor : 0.02;
        extent = d3.extent(data, key);
        range = extent[1] - extent[0];
        offset = range * 0.05;
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
                     

        container.select(".tooltip")
                    .style("opacity", 1)
                    .style("left", tooltipLeft + "px")
                    .style("top", tooltipTop + "px");
    }

    function zoom() {
      circle.attr("transform", transform);
    }

    function transform(d) {
      return "translate(" + xScale(xValue(d)) + "," + yScale(yValue(d)) + ")";
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

    // This allows other objects to 'listen' to events dispatched by the _table object.
    d3.rebind(chart, dispatcher, "on");
    
    return chart;
}