for (var i=0; i<glimma.linkage.length; i++) {
	(function () {
		var from = glimma.linkage[i].from - 1;
		var to = glimma.linkage[i].to - 1;
		var src = glimma.linkage[i].src;
		var dest = glimma.linkage[i].dest;

		glimma.charts[from].on(src + ".chart" + from, function (d) {
			glimma.charts[to][dest](d);
		});	
	}());
}