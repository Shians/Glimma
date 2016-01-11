interactiveMDSplot <- function(x, ...) {
  UseMethod("interactiveMDSplot")
}

glMDSplot.default <- function(x, col, top=500, labels = NULL, gene.selection = "pairwise", dir=NULL, launch=TRUE, main=NULL) {
	#------------------------------------------------------------
  	# Check for errors
  	#------------------------------------------------------------

	x <- as.matrix(x)
	nsamples <- ncol(x)

	if (nsamples < 3) {
		stop(paste("Only", nsamples, "columns of data: need at least 3"))
	}

	cn <- colnames(x)
	bad <- rowSums(is.finite(x)) < nsamples

	if (any(bad)) {
		x <- x[!bad, drop=FALSE]
	}

	nprobes <- nrow(x)
	top <- min(top, nprobes)

	if (is.null(labels)) {
		labels <- paste("Sample", 1:nsamples)
	} else {
		labels <- as.character(labels)
	}

	#------------------------------------------------------------
	# Process input
	#------------------------------------------------------------
	# If no output directory is provided, set to current working directory
	if (is.null(dir)) {
		dir <- getwd()
	}

	# Locate files in package library
	page.path <- system.file("report_page", package="Glimma")
	files <- paste(page.path, c("mds_plot.js", "mds_plot.html", "plot_styles.css", "mds_styles.css", "utilities.js", "js", "css"), sep="/")

	# Make report page directory and copy over data
	if (substr(dir, nchar(dir), nchar(dir)) == "/") {
		dir.create(paste(dir, "report_page", sep=""), showWarnings=FALSE)
		report.path <- paste(dir, "report_page", sep="")
	} else {
		dir.create(paste(dir, "report_page", sep="/"), showWarnings=FALSE)
		report.path <- paste(dir, "report_page", sep="/")
	}
	file.copy(from=files, to=report.path, recursive=TRUE)

	data.filename <- paste(report.path, "/mds_data.js", sep="")

	gene.selection <- match.arg(gene.selection, c("pairwise", "common"))
	dd <- matrix(0, nrow=nsamples, ncol=nsamples, dimnames=list(cn,cn))
	if (gene.selection == "pairwise") {
		topindex <- nprobes - top + 1L
		for (i in 2L:(nsamples)) {
			for (j in 1L:(i - 1L)) {
				dd[i, j] = sqrt(mean(sort.int((x[, i] - x[, j])^2, partial = topindex)[topindex:nprobes]))
			}
		}
	} else if (gene.selection == "common") {
		if (nprobes > top) {
			s <- rowMeans((x -  rowMeans(x))^2)
			o <- order(s, decreasing=TRUE)
			x <- x[o[1L:top], , drop=FALSE]
		}
		for (i in 2L:nsamples) {
			dd[i, 1L:(i - 1L)] = sqrt(colMeans((x[, i] - x[, 1:(i - 1), drop = FALSE])^2))
		}
	}
	# Calculated MDS coordinates
	a1 <- suppressWarnings(cmdscale(as.dist(dd), k=min(10, nsamples-1), eig=TRUE))

	dmatrix <- a1$points
	eigvals <- round(a1$eig, 3)[1:10]
	eigvals[is.na(eigvals)] <- 0
	eigsum <- round(sum(a1$eig), 3)
	
	# Function to convert colour strings into hex codes
	CharToHexCol <- function(x) {
		out <- apply(as.character(as.hexmode(col2rgb(x, alpha=FALSE))), 2, function(x) {paste0("#", paste0(x, collapse=""))})
		sapply(out, function(x) { ifelse(x=="#000", "#000000", x) })
	}

	# Functions to convert numbers into corresponding hex codes for colours
	NumToHexCol <- function(x) {
		col <- palette()[as.integer(col)]
		out <- apply(as.character(as.hexmode(col2rgb(x, alpha=FALSE))), 2, function(x) {paste0("#", paste0(x, collapse=""))})
		sapply(out, function(x) { ifelse(x=="#000", "#000000", x) })
	}

	if (is.null(dim(col))) {
		if (is.character(col)) {
			cols <- ChartoHexCol(col)
		} else if (is.factor(col) || is.numeric(col)) {
			cols <- NumToHexCol(col)
		} else {
			stop("input col must not of class 'interger', 'factor' or 'character'")
		}		
	} else {
		matdim <- dim(col)
		factors <- colnames(col)
		if (is.character(col)) {
			cols <- matrix(ChartoHexCol(col), matdim[1], matdim[2])
			colnames(cols) <- factors
		} else if (is.factor(col) || is.numeric(col)) {
			cols <- matrix(NumToHexCol(col), matdim[1], matdim[2])
			colnames(cols) <- factors
		} else {
			stop("input col must not of class 'interger', 'factor' or 'character'")
		}
	}
	
	#------------------------------------------------------------
	# Generate javascript strings
	#------------------------------------------------------------
	
	if (is.null(dim(col))) {
		multicol <- quotify("false")
		col <- objectify("col", arrayify(cols))
	} else {
		multicol <- quotify("true")
		col <- objectify(colnames(cols), apply(cols, 2, arrayify))
	}

	dmatrix <- data.frame(dmatrix)
	colnames(dmatrix) <- as.character(1:ncol(dmatrix))
	dimjs <- makeDFJson(dmatrix)

	labeljs <- arrayify(quotify(labels))

	eigjs <- arrayify(paste(eigvals, collapse=","))

	plot.title <- quotify(main)

	#------------------------------------------------------------
  	# Output
  	#------------------------------------------------------------
  
	data.filename <- paste(report.path, "/mds_data.js", sep="")
	printJsonToFile(c(dimjs, labeljs, col, eigjs, eigsum, plot.title), filename=data.filename,
					varname=c("dim", "lab", "col", "eigenVals", "eigenSum", "pageTitle"))

}
