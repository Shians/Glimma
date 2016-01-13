//* REQUIRES BOOTSTRAP.JS LIBRARY *//

glimma.layout = {};

glimma.layout.bsAddRow = function(selection) {
	var row = selection.append("div").attr("class", "row");
	return row;
};

glimma.layout.bsAddCol = function(selection, type, size) {
	var col = selection.append("div").attr("class", "col-" + type + "-" + size);
	return col;
};

// Function to add an empty p element to a selection with the classes required
// for glimma plots to find.
glimma.layout.addPlottableWindow = function(selection) {
	var p = selection.append("p").classed("glimma-plot", true).classed("available", true);
	return p;
};

glimma.layout.setupRow = function(selection, type, sizes) {
	var arrSum = function (array) {
		return array.reduce(function (a, b) { return a + b; });
	};

	if (arrSum(sizes) <= 12 && arrSum(sizes) > 0) {
		var row = glimma.layout.bsAddRow(selection);
		for (var i = 0; i < sizes.length; i++) {
			var col = glimma.layout.bsAddCol(row, type, sizes[i]);
			glimma.layout.addPlottableWindow(col);
		}
	}

	return selection;
};

glimma.layout.setupGrid = function(selection, type, dim) {
	var rows = +dim[0],
		cols = +dim[1],
		size = Math.floor(12 / cols),
		sizes = [];

	if (cols < 1) {
		return null;
	}

	for (var i = 0; i < cols; i++) {
		sizes.push(size);
	}

	for (var i = 0; i < rows; i++) {
		glimma.layout.setupRow(selection, type, sizes);
	}
};