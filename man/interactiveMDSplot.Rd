\title{Interactive version of plotMDS (Multidimensional scaling plot) from the limma package}
\name{interactiveMDSplot}
\alias{interactiveMDSplot}
\alias{interactiveMDSplot.default}

\description{
This function produces an interactive two-dimensional scatterplot so that distances on the plot approximate 
the typical log2 fold changes between the samples.
}

\usage{
interactiveMDSplot(x, col, top = 500, labels = NULL, gene.selection = "pairwise", dir = NULL, launch=TRUE, main=NULL)
}

\arguments{
  \item{x}{any data object which can be coerced to a matrix, such as \code{ExpressionSet} or \code{EList}.}
  \item{top}{number of top genes used to calculate pairwise distances.}
  \item{labels}{character vector of sample names or labels. Defaults to \code{colnames(x)}.}
  \item{gene.selection}{character, \code{"pairwise"} to choose the top genes separately for each pairwise comparison between the samples or \code{"common"} to select the same genes for all comparisons.}
  \item{dir}{character string indicating the output directory for the plot.}
  \item{launch}{\code{logical}. Should the resulting plot be displayed?}
  \item{main}{character string specifying the title of the html page.}
}

\details{
This function generates an interactive MDS (multdimensional scaling) plot, based on the \code{plotMDS} function from the limma package.

This plot is a variation on the usual multidimensional scaling (or principle coordinate) plot, in that a distance measure particularly appropriate for the microarray context is used.
The distance between each pair of samples (columns) is the root-mean-square deviation (Euclidean distance) for the top \code{top} genes.
Distances on the plot can be interpreted as \emph{leading log2-fold-change}, meaning
the typical (root-mean-square) log2-fold-change between the samples for the genes that distinguish those samples.

If \code{gene.selection} is \code{"common"}, then the top genes are those with the largest standard deviations between samples.
If \code{gene.selection} is \code{"pairwise"}, then a different set of top genes is selected for each pair of samples.
The pairwise feature selection may be appropriate for microarray data when different molecular pathways are relevant for distinguishing different pairs of samples.
}

\value{
An html page with an interactive MDS plot in which the dimensions plotted can be changed by the user.
}

\author{Shian Su}

\references{
Ritchie, ME, Phipson, B, Wu, D, Hu, Y, Law, CW, Shi, W, and Smyth, GK (2015).
limma powers differential expression analyses for RNA-sequencing and microarray studies.
\emph{Nucleic Acids Research} 43, e47.
\url{http://nar.oxfordjournals.org/content/43/7/e47}
}

\seealso{
\code{\link{plotMDS}} from the limma package, \code{interactiveMDplot}.
}

\examples{
\dontrun{
  library(Glimma)
  library(edgeR)
  load("x.rda")
  # RNA-seq data set available from GEO under accession number GSE64099
  # filter out genes with low read counts
  sel = rowSums(cpm(x$counts)>0.5)>=3
  x = x[sel,]
  x$genes = x$genes[,c(1,3)]
  des = model.matrix(~x$samples$group)
  colnames(des)[2] = "Smchd1nullvsWt"
  x = calcNormFactors(x, method="TMM")
  genotype = x$samples$group

  # See how replicate sample cluster
  interactiveMDSplot(x, labels=1:7, col=as.numeric(genotype), main="MDS plot for Smchd1 experiment")
}
}
