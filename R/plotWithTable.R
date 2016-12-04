# Hidden internal functions for use by edgeR and limma based plotting
plotWithTable <- function(plotting.data, sample.exp, display.columns,
                            search.by, id.column, default.col, jitter,
                            table, path, folder, html, launch,
                            xval, yval, xlab, ylab, side.xlab, side.ylab,
                            side.log, side.gridstep,
                            ...) {

    # Reordering so that significant points appear on top of insignificant
    # points.
    plotting.data <- rbind(plotting.data[plotting.data$col == default.col, ],
                           plotting.data[plotting.data$col != default.col, ])

    plot1 <- glScatter(plotting.data, xval=xval, yval=yval,
                    xlab=xlab, idval=id.column,
                    ylab=ylab,
                    annot=c(display.columns, xval, yval, "Adj.PValue"),
                    flag="mdplot", ndigits=4, info=list(search.by=search.by),
                    ...)

    if (!is.null(sample.exp)) {
        link1 <- gllink(1, 2, "hover", "yChange", flag="byKey", info=id.column)
        link2 <- gllink(1, 2, "click", "yChange", flag="byKey", info=id.column)
        plot2 <- glScatter(sample.exp, xval="Group",
                        yval=colnames(sample.exp)[4],
                        idval="Sample", xlab=side.xlab, ylab=side.ylab,
                        main=colnames(sample.exp)[4],
                        annot=c("Sample", colnames(sample.exp)[4]),
                        colval="col", log=ifelse(side.log, "y", ""),
                        annot.lab=c("Sample", side.ylab), x.jitter = jitter,
                        ndigits=4, hide=TRUE, ystep=side.gridstep,
                        ygrid=TRUE)
    } else {
        link1 <- NULL
        link2 <- NULL
        plot2 <- NULL
    }

    draw.plots(table, display.columns, search.by, xval, yval,
                plot1, plot2, link1, link2, path, folder, html,
                launch)
}

draw.plots <- function(table, display.columns, search.by, xval, yval,
                        plot1, plot2, link1, link2, path, folder, html,
                        launch) {
    if (!is.null(plot2)) {
        if (table) {
            # TODO: Have different columns to tooltip
            link3 <- gltablink(1, 1, action="highlightById")
            table1 <- glTable(1, plot1$anno)
            glimma(plot1, plot2, link1, link2, table1, link3, layout=c(1, 2),
                path=path, folder=folder, html=html, overwrite=TRUE,
                launch=launch)
        } else {
            button1 <- glAutoinput(1, "highlightBySearch", search.by)
            glimma(plot1, plot2, button1, link1, link2, layout=c(1, 2),
                path=path, folder=folder, html=html, overwrite=TRUE,
                launch=launch)
        }
    } else {
        if (table) {
            link3 <- gltablink(1, 1, action="highlightById")
            table1 <- glTable(1, plot1$anno)
            glimma(plot1, table1, link3, layout=c(1, 1),
                path=path, folder=folder, html=html, overwrite=TRUE,
                launch=launch)
        } else {
            button1 <- glAutoinput(1, "highlightBySearch", search.by)
            glimma(plot1, button1, layout=c(1, 1),
                path=path, folder=folder, html=html, overwrite=TRUE,
                launch=launch)
        }
    }
}
