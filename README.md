# Glimma
Welcome to Glimma, a R package for creating interactive plots for differential expression analysis.

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

### Using Devtools
The developmental version of the package can also be installed directly from GitHub using the [devtools](https://github.com/hadley/devtools) package.

```r
install.packages("devtools")
devtools::install_github("Shians/Glimma")
```
