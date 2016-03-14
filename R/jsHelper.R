## 
# Helper functions for writing javascript

writeMaker <- function(filename) {
    function (string) {
        f <- cat
        formals(f)$file <- filename
        formals(f)$append <- TRUE
        f(string)
    }
}

# Function to write the opening tag for BootStrap.js row
bsRowStart <- function() {
    writeMaker("<div class=\"row\">")
}

# Function to write the closing tag for BootStrap.js row
bsRowEnd <- function() {
    writeMaker("</div>")
}

# Function to write a column cell for BootStrap.js
bsCol <- function(size, class="plot-device", type="md") {
    col.type <- quotify(paste("col", size, type, sep="-"))
    tag <- paste0("<div class=", col.type, "></div>")
    writeMaker(tag)
}
