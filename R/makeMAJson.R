makeMAJson <- function(x, ...) {
  UseMethod("makeMAJson")
}

# Default method
makeMAJson.default <- function(x, xvals=NULL, yvals=NULL, id=NULL, anno=NULL) {
  if (ncol(x) < 2) {
    stop("at least 2 columns are required")
  }
  
  if (is.null(id)) {
    id <- paste("ID", 1:nrow(x))
  }

  if (is.null(anno)) {
    
  }
}

# Function to convert the aveExp, LogFc, symb, p.value and decideTest of MArrayLM
# to json format
makeMAJson.MArrayLM <- function(x, genes=NULL, coef=NULL, p.value=0.05, adjust.method="BH") {
  if (is.null(coef)) {
    stop("coef not specified")
  }

  if (!is(x, "MArrayLM")) {
    stopType("MArrayLM")
  }

  req <- c("coefficients", "Amean", "p.value")
  if (any(is.na(match(req, names(x))))) {
    str <- paste(req[is.na(match(req, names(x)))], collapse=", ")
    stop(paste("data for", str, "missing"))
  }

  # Function to convert (-1, 0, 1) to (red, black, green)
  toCol <- function(num) {
    if (num == 0) {
      return("black")
    } else if (num == -1) {
      return("red")
    } else if (num == 1) {
      return("green")
    } else {
      stop("value that is not -1, 0 or 1 detected")
    }
  }

  # Pull out relevant data columns and assemble into data frame
  GeneID <- names(x$Amean)
  LogFC <- as.numeric(x$coefficients[, coef])
  AvgExpr <- as.numeric(x$Amean)
  Log2Sigma <- as.numeric(log2(x$sigma))
  # Take the selected column of design matrix to perform test on
  col <- sapply(decideTests(x[,coef], p.value=p.value, adjust.method=adjust.method), toCol)
  if (!is.null(genes)) {
    symb <- genes  
  } else if (!is.null(x$genes$Symbols)) {
    symb <- as.character(x$genes$Symbols)
  } else {
    stop("no gene symbols specified")
  }
  pval <- p.adjust(x$p.value[, coef], method=adjust.method)
  dframe <- data.frame(cbind(GeneID, LogFC, AvgExpr, Log2Sigma, col, symb, pval))

  # Convert to character then numeric to ensure factors are converted correctly
  for (col in c("LogFC", "AvgExpr", "pval", "Log2Sigma")) {
    dframe[[col]] <- as.numeric(as.character(dframe[[col]]))
  }

  dframe$symb <- as.character(dframe$symb)

  dframe <- dframe[order(dframe$col), ]

  nUndef <- sum(is.na(dframe$symb))
  undefNames <- paste0("unnamed", 1:nUndef)
  dframe$symb[is.na(dframe$symb)] <- undefNames

  output <- makeDFJson(dframe)

  output
}