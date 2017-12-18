#' Glimma plot manager
#'
#' Core glimma plot manager. Generates environment for glimma plots.
#'
#' @param ... the jschart or jslink objects for processing.
#' @param layout the numeric vector representing the number of rows and columns in plot window.
#' @param path the path in which the folder will be created.
#' @param folder the name of the fold to save html file to.
#' @param html the name of the html file to save plots to.
#' @param overwrite the option to overwrite existing folder if it already exists.
#' @param launch TRUE to launch plot after call.
#'
#' @return Generates interactive plots based on filling layout row by row from left to right.
#'
#' @examples
#' data(iris)
#' \donttest{
#' plot1 <- glScatter(iris, xval="Sepal.Length", yval="Sepal.Width", colval="Species")
#' glimma(plot1, c(1,1))
#' }
#'
#' @importFrom utils browseURL read.delim

glimma <- function(..., layout=c(1, 1), path=getwd(), folder="glimma-plots",
                html="index", overwrite=TRUE, launch=TRUE) {
    nplots <- 0

    ##
    # Input checking
    for (i in list(...)) {
        if (class(i) == "jschart") {
            nplots <- nplots + 1
        }
    }

    if (!is.numeric(layout) || !(length(layout) == 2)) {
        stop("layout must be numeric vector of length 2")
    }

    if (layout[2] < 1 || layout[2] > 6) {
        stop("number of columns must be between 1 and 6")
    }

    if (nplots > layout[1] * layout[2]) {
        stop("More plots than available layout cells")
    }

    if (overwrite == FALSE) {
        if (file.exists(file.path(path, folder))) {
            stop(paste(file.path(path, folder), "already exists"))
        }
    }

    if (!file.exists(path)) {
        stop(paste(path, "does not exist"))
    }
    #
    ##

    # Normalise input
    folder <- sanitisePath(folder)
    layout <- round(layout)

    # Convert variable arguments into list
    args <- list(...)

    # Create folder
    if (!dir.exists(file.path(path, folder))) {
        dir.create(file.path(path, folder))
    }

    # Create file
    index_path <- system.file(package="Glimma", "index.html")
    js_path <- system.file(package="Glimma", "js")
    css_path <- system.file(package="Glimma", "css")

    # Renaming the data.js in html file
    temp <- gsub(
        "data.js",
        paste0(html, ".js"),
        read.delim(index_path, header=FALSE, as.is=TRUE, sep="\n")[, 1]
    )

    cat(
        temp, 
        file = file.path(path, folder, paste0(html, ".html")),
        sep = "\n"
    )

    file.copy(
        js_path,
        file.path(path, folder),
        recursive = TRUE,
        overwrite = overwrite
    )

    file.copy(
        css_path,
        file.path(path, folder),
        recursive = TRUE,
        overwrite = overwrite
    )

    data.path <- file.path(path, folder, "js", paste0(html, ".js"))
    cat("", file=data.path, sep="")
    write.data <- writeMaker(data.path)

    # Generate layout
    layout_method <- jsMethod("glimma", "layout", "setupGrid")
    layout_args <- jsArgs(
        "d3.select(\".container\")",
        quotify("md"),
        arrayify(layout)
    )
    layout <- jsCall(layout_method, layout_args)
    write.data(layout)

    processed_args <- processArguments(args, write.data)
    actions <- processed_args$actions
    inputs <- processed_args$inputs

    # Write linkage
    if (not.null(actions)) {
        actions_js <- makeJson(actions)
        write.data(paste0("glimma.storage.linkage = ", actions_js, ";\n"))
    } else {
        write.data("glimma.storage.linkage = [];\n")
    }

    # Write input fields
    if (not.null(inputs)) {
        inputs_js <- makeJson(inputs[-1, ])
        write.data(paste0("glimma.storage.input = ", inputs_js, ";\n"))
    } else {
        write.data("glimma.storage.input = [];\n")
    }

    # Launch page if required
    if (launch) {
        browseURL(file.path(path, folder, paste0(html, ".html")))
    }
}

# process arguments to glimma
processArguments <- function(args, write.data) {
    is_accepted_class <- function(x) {
        x %in% c("jslink", "jschart", "jsinput", "jstable")
    }

    # append object to list
    list_append <- function(lst, obj) {
        lst[[length(lst) + 1]] <- obj
        lst
    }

    action_list <- list()
    input_list <- list()

    # Process arguments
    for (i in seq_along(args)) {
        obj_class <- class(args[[i]])
        if (is_accepted_class(obj_class)) {
            obj_type <- args[[i]]$type
            if (obj_type == "link") {
                # add plot linkage action
                action_list <- list_append(action_list, args[[i]]$link)
            } else if (obj_type == "tablink") {
                # add table linkage
                action_list <- list_append(action_list, args[[i]]$link)
            } else if (obj_type == "autocomplete") {
                # add autocomplete linkage
                input_list <- list_append(input_list, args[[i]]$input)
            } else if (obj_type == "data.table") {
                processTable(write.data, args[[i]])
            } else {
                processPlot(write.data, obj_type, args[[i]], i)
            }
        }
    }

    actions <- do.call(rbind, action_list)
    inputs <- do.call(rbind, input_list)

    list(
        actions = actions,
        inputs = inputs
    )
}

# Helper function for parsing the information in a plot object
processPlot <- function(write.data, type, chart, index) {
    # Write json data
    write.data(
        jsCall(
            jsMethod("glimma", "storage", "chartData", "push"),
            jsCall(
                jsMethod("glimma", "transform", "toRowMajor"),
                chart$json
            )
        )
    )

    # Write plot information
    chart$json <- NULL
    chartInfo <- makeJson(chart)
    write.data(paste0("glimma.storage.chartInfo.push(", chartInfo, ");\n"))

    # Write plot call
    if (type == "scatter") {
        constructScatterPlot(chart, index, write.data)
    } else if (type == "bar") {
        constructBarPlot(chart, index, write.data)
    }
}

processTable <- function(write.data, data.table) {
    # Creates glimma.chart.table()
    call1_func <- jsMethod("glimma", "chart", "table")
    call1 <- jsCall(call1_func, "")

    # Creates data()
    call2_func <- jsFunction("data")
    call2_arg <- paste0("glimma.storage.chartData[", data.table$input-1, "]")
    call2 <- jsCall(call2_func, call2_arg)

    # Creates columns()
    call3_func <- jsFunction("columns")
    call3_arg <- arrayify(quotify(data.table$columns))
    call3 <- jsCall(call3_func, call3_arg)

    # glimma.storage.tables.push( glimma.chart.table().data().columns() )
    call4_func <- jsMethod("glimma", "storage", "tables", "push")
    call4_arg <- jsChain(call1, call2, call3)
    call4 <- jsCall(call4_func, call4_arg)
    write.data(call4)

    # glimma.layout.bsAddRow
    call5_func <- jsMethod("glimma", "layout", "bsAddRow")
    call5_arg <- jsArgs("d3.select(\".container\")")
    call5 <- jsCall(call5_func, call5_arg)

    call6_func <- jsMethod("glimma", "layout", "addTable")
    call6 <- jsCall(call6_func, call5)
    write.data(call6)
}

sanitisePath <- function(folder) {
    output <- folder

    if (lastChar(folder) == "/" || lastChar(folder) == "\\") {
        folder <- substring(folder, 1, nchar(folder) - 1)
    }

    output
}
