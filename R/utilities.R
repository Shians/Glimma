# Helper function for printing type errors
stopType <- function(type, name="x") {
  type <- quotify(type)
  name <- quotify(name)
  stop(paste("input", name, "not of class", type))
}

# Function to add double quotes onto the start and end of the strings
quotify <- function(x) {
  paste("\"", x, "\"", sep="")
}

# Function to add square brackets around string
arrayify <- function(x) {
  paste("[", paste(x, collapse=","), "]", sep="")
}

# Function to add square brackets around string
objectify <- function(x, y) {
  paste("{", paste(x, y, sep=":", collapse=","), "}", sep="")
}

#' Create json from data.frame
#' 
#' @param df A data.frame object
#' @return character string containing data.frame in json format
#' @examples
#' data(iris)
#' makeDFJson(iris)

makeDFJson <- function(df) {
  df <- data.frame(df)

  for (n in names(df)) {
    if (!is(df[[n]], "numeric") && !is(df[[n]], "logical")) {
      df[[n]] <- quotify(as.character(df[[n]]))
    }
  }

  coln <- paste(quotify(colnames(df)), ":", sep="")
  temp <- t(apply(df, 1, function (x) { paste(coln, x, sep="") }))
  temp <- apply(temp, 1, function (x) { paste("{", paste(x, collapse=","), "}", sep="") })

  output <- paste("[", paste(temp, collapse=","), "]", sep="")
  class(output) <- "json"

  output
}

# Function to make json object from a chart, ignoring the json property
makeChartJson <- function(chart) {
  if (!is(chart, "jschart")) {
    stopType("jschart", "chart")
  }

  makeEntry <- function(d) {
    if (is.null(chart[[d]])) {
      return(paste(quotify(d), "null", sep=":"))
    } else if (length(chart[[d]]) > 1) {
      return(paste(quotify(d), arrayify(quotify(chart[[d]])), sep=":"))
    } else {
      return(paste(quotify(d), quotify(chart[[d]]), sep=":"))
    }
  }

  chart$json <- NULL

  paste0("{", paste(sapply(names(chart), makeEntry), collapse=","), "}")
}

# Function to print stored json as a javascript var declaration
printJsonToFile <- function(json, filename, varname) {
  file.con <- file(description=filename, open="w")

  if (length(json) != length(varname)) {
    stop("json vector must be same length as varname vector")
  }

  for (i in 1:length(json)) {
    write(paste0("var ", varname[i], " = ", json[i], ";"), file=file.con,
          sep=" ", append=TRUE)
  }

  close(file.con)
}

# Function to make json object ouf of factor levels
makeFactJson <- function(sample.groups) {
  sample.groups <- as.factor(sample.groups)
  l <- levels(sample.groups)
  l <- paste("\"", l, "\"", sep="")
  paste("[", paste(l, collapse=", "), "]", sep="")
}

# Function to get the nth character of a string
char <- function(string, n) {
  if (!is(string, "character")) {
    stopType("string", "character")
  }

  if (any(abs(n) > nchar(string))) {
    stop(paste(quotify("n"), "is outside index range"))   
  }

  if (n == 0) {
    return(rep("", length(string)))
  } 

  if (n < 0) {
    n <- nchar(string) + n + 1
  }

  return(substr(string, n, n))
}

# Function to return filepaths
pathMaker <- function(path) {
  if (char(path, -1) != "/") { 
    stop("path must end with /")
  }
  function (x) {
    return(paste0(path, x))
  }
}
