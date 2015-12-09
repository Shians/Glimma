\name{interactiveMAplot}
\alias{interactiveMAplot}

\title{
	Interactive version of plotMA from the limma package.
}
\description{
	This function produces a MA plot that the user can search for genes, plot counts per million and SA plot.
}
\usage{
interactiveMAplot(object, y, p.value=0.05, lfc=0, adjust.method="BH", groups, labels=NULL, coef=NULL,
                  searchBy="Symbols", dir=NULL, launch=TRUE, main=NULL, url=NULL, urlGeneIDs=NULL, displayGeneIDs="Symbols")
}
\arguments{
  \item{object}{\code{MArrayLM} object output from \code{lmFit} or \code{contrasts.fit} containing logFCs (M-values) and average expression values (A-values) to be plotted.}
  \item{y}{a \code{RGList}, \code{MAList}, \code{EList} or \code{ExpressionSet} containing pre-processed data (both expression and annotation) before linear model has been 
           fitted, used to plot individual expression values in a second panel.}
  \item{groups}{character vector or factor specifying the experimental groups (used to separate expression values for plotting).}
  \item{p.value}{numeric value between 0 and 1 giving the desired size of the test.}
  \item{lfc}{minimum log2-fold-change required to highlight points.}
  \item{adjust.method}{method used to adjust the p-values for multiple testing.  Options, in increasing conservatism, include \code{"none"}, \code{"BH"}, \code{"BY"} and \code{"holm"}.
        See \code{\link[stats]{p.adjust}} for details.}
  \item{labels}{character vector specifying sample labels.}
  \item{coef}{numeric scalar indicating which coefficient from the linear model to make an MA plot of (defaults to first coefficient).}
  \item{searchBy}{character string specifying gene annotation column to allow searching by in final plot.}
  \item{dir}{character string indicating the output directory for the plot.}
  \item{launch}{\code{logical}. Should the resulting plot be displayed?}
  \item{main}{character string specifying the title of the html page.}
  \item{url}{character string specifying URL to use to link out to external gene ID information.}
  \item{urlGeneIDs}{character string specifying the gene annotation column to use as key at specified \code{url}.}
  \item{displayGeneIDs}{character string (or vector) specifying the gene annotation column/(s) to display in plot.}
}

\details{
  This function generates an interactive MA plot.
}

\value{A html page with a searchable MA plot, with a second panel displyaing either the expression values for 
       a selected gene or an SA plot summarising variability as a function of abundance with the particular 
       gene of interest highlighted.}

\references{
  Ritchie, M.E., Phipson, B., Wu, D., Hu, Y., Law, C.W., Shi, W., and Smyth, G.K. (2015). 
  limma powers differential expression analyses for RNA-sequencing and microarray studies. 
  Nucleic Acids Research 43, doi: 10.1093/nar/gkv007.
}

\author{Shian Su, Matt Ritchie}

\seealso{
	\code{\link{interactivebarcodeplot}}
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
  plotMDS(x, labels=1:7, col=as.numeric(genotype), main="MDS plot for Smchd1 experiment")
  legend("topright", legend=c("WT", "Smchd1 null"), pch=15, col=1:2)

  # Apply voom with sample quality weights
  v = voomWithQualityWeights(x, design=des, normalization="none", plot=TRUE)
  vfit = lmFit(v)
  vfit = eBayes(vfit)
  topTable(vfit,coef=2,sort.by="P")

  # Make interactive MA plot of results for coefficient of interest (Smchd1 null vs Wt)
  interactiveMAplot(vfit, v, groups=genotype, coef=2)
}
}
\keyword{hplots}
\keyword{graphics}
