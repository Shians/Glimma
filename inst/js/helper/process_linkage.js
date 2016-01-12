for (var i=0; i<glimma.linkage.length; i++) {
	var from = glimma.linkage[i].from - 1;
	var to = glimma.linkage[i].to - 1;
	var action = glimma.linkage[i].action;

	glimma.charts[from].on(action + ".chart" + from, function (d) {
		glimma.charts[to][action](d);
	});
}