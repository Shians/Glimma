# Hidden internal functions for use by edgeR and limma based plotting
plotWithTable <- function(
    plotting_data,
    sample_exp,
    display.columns,
    main,
    side.main,
    default.col,
    jitter,
    path,
    folder,
    html,
    launch,
    xval,
    yval,
    xlab,
    ylab,
    side.xlab,
    side.ylab,
    side.log,
    side.gridstep,
    ...
) {

    # Reordering so that significant points appear on top of insignificant
    # points.
    plotting_data <- rbind(
        plotting_data[plotting_data$cols == default.col, ],
        plotting_data[plotting_data$cols != default.col, ]
    )

    plot1 <- glScatter(
        plotting_data,
        xval = xval,
        yval = yval,
        main = main,
        idval = side.main,
        xlab = xlab,
        ylab = ylab,
        annot = c(display.columns, xval, yval, "Adj.PValue"),
        flag = "mdplot",
        ndigits = 4,
        ...
    )

    if (not.null(sample_exp)) {
        link1 <- gllink(1, 2, "hover", "yChange", flag="byKey", info=side.main)
        link2 <- gllink(1, 2, "click", "yChange", flag="byKey", info=side.main)
        plot2 <- glScatter(
            sample_exp,
            xval = "Group",
            yval = colnames(sample_exp)[4],
            idval = "Sample",
            xlab = side.xlab,
            ylab = side.ylab,
            main = colnames(sample_exp)[4],
            annot = c("Sample", colnames(sample_exp)[4]),
            colval = "cols",
            log = ifelse(side.log, "y", ""),
            annot.lab = c("Sample", side.ylab),
            x.jitter = jitter,
            ndigits = 4,
            hide = TRUE,
            ystep = side.gridstep,
            ygrid = TRUE
        )
    } else {
        link1 <- NULL
        link2 <- NULL
        plot2 <- NULL
    }

    draw.plots(
        display.columns = display.columns,
        xval = xval,
        yval = yval,
        plot1 = plot1,
        plot2 = plot2,
        link1 = link1,
        link2 = link2,
        path = path,
        folder = folder,
        html = html,
        launch = launch
    )
}

draw.plots <- function(
    display.columns,
    xval,
    yval,
    plot1,
    plot2,
    link1,
    link2,
    path,
    folder,
    html,
    launch
) {
    link3 <- gltablink(1, 1, action="highlightById")
    table1 <- glTable(1, plot1$anno)

    if (not.null(plot2)) {
        glimma(
            plot1,
            plot2,
            link1,
            link2,
            table1,
            link3,
            layout = c(1, 2),
            path = path,
            folder = folder,
            html = html,
            overwrite = TRUE,
            launch = launch
        )
    } else {
        glimma(
            plot1,
            table1,
            link3,
            layout = c(1, 1),
            path = path,
            folder = folder,
            html = html,
            overwrite = TRUE,
            launch = launch
        )
    }
}
