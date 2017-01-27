[![Build Status](https://travis-ci.org/Shians/Glimma.svg?branch=master)](https://travis-ci.org/Shians/Glimma)

# Glimma
Welcome to Glimma, a R package for creating interactive plots for differential expression analysis.

User suggestions are very welcome, please start an issue for usability improvements or features you'd like to see!

# Installation

### Using BioConductor
This is the recommended method for installing Glimma. The release version can be installed by running the code below:

```r
source("https://bioconductor.org/biocLite.R")
biocLite("Glimma")
```

If a more recent version is required the developmental version:

```r
source("https://bioconductor.org/biocLite.R")
useDevel()
biocLite("Glimma")
```

# Features
## MDS Plots
### Changing dimensions to plots
![bar-click](https://raw.githubusercontent.com/shians/glimma/master/images-doc/MDS-click.gif)

### Changing conditions to colour by
![groups](https://raw.githubusercontent.com/shians/glimma/master/images-doc/MDS-groups.gif)

## MD Plot
### Hovering over points
![points-hover](https://raw.githubusercontent.com/shians/glimma/master/images-doc/point-hover.gif)

### Clicking points
![points-click](https://raw.githubusercontent.com/shians/glimma/master/images-doc/point-click.gif)

### Clicking on table entries
![table-click](https://raw.githubusercontent.com/shians/glimma/master/images-doc/table-search.gif)
