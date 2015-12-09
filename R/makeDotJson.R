makeDotJson <- function(x, sample.grous, ...) {
  UseMethod("makeDotJson")
}

# Function to generate JSON for dot plot of samples
makeDotJson.EList <- function(x, sample.groups, sample.names=NULL) {
  if (!is(x, "EList")) {
    stopType("EList")
  }

  sample.groups <- as.factor(sample.groups)

  expr <- x$E
  if (!is.null(sample.names)) {
    if (length(sample.names) == length(colnames(expr))){
      colnames(expr) <- sample.names
    } else {
      stop("sample names must be vector of same length as number of columns
            of data")
    }
  } else {
    sample.names <- colnames(expr)
  }

  geneID <- rownames(expr)

  ar.len <- length(geneID)
  overall.array <- character(ar.len)
  a.ind <- 1

  for (id in 1:length(geneID)) {
    len <- length(sample.groups)
    c.array <- character(length=len)

    for (sam in 1:len) {
      vstr <- sprintf("\"value\": %.6f", expr[id, sam])
      sstr <- sprintf("\"sample\": \"%s\"", sample.names[sam])
      gstr <- sprintf("\"group\": \"%s\"", as.character(sample.groups[sam]))
      objstr <- paste(vstr, sstr, gstr, sep=", ")
      objstr <- paste0("{", objstr, "}")
      c.array[sam] <- objstr
    }

    str <- paste0(c.array, collapse = ", ")
    str <- sprintf("\"%s\": [%s]", geneID[id], str)
    overall.array[a.ind] <- str
    a.ind <- a.ind + 1

  }

  output <- paste0(overall.array, collapse = ", ")
  output <- paste0("{", output, "}")

  class(output) <- "json"

  output
}