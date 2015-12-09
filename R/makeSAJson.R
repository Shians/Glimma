makeSAJson <- function(x, ...) {
  UseMethod("makeSAJson")
}

# Default method
makeSAJson.default <- function(x) {
  
}

# Function to generate JSON for SA plot from
makeSAJson.MArrayLM <- function(fit) {
  if (!is(fit, "MArrayLM"))
    stopType("MArrayLM", "fit")
  x <- fit$Amean
  y <- log2(fit$sigma)

  lowessData <- lowess(x, y, f = 0.4)

  end <- length(lowessData$x)
  gap <- round(end/200)
  ind <- c(seq(1, end, gap), end)

  x_sampled <- lowessData$x[ind]
  y_sampled <- lowessData$y[ind]

  len <- length(x_sampled)
  arr <- character(length=len)
  for (i in 1:len) {
    x_co <- x_sampled[i]
    y_co <- y_sampled[i]
    arr[i] <- sprintf("{\"x\": %.6f, \"y\": %.6f}", x_co, y_co)
  }

  output <- paste(arr, collapse = ", ")
  output <- paste0("[", output, "]")

  class(output) <- "json"

  output
}
