# Function to generate relevant json objects given EList and MAarrayLM
createMAJson <- function(MArrayLM, Elist, sample.groups, genes, labels, p.value=p.value,
                        baseURL="http://www.ncbi.nlm.nih.gov/gene/?term=", searchBy="Symbol", 
                        linkBy="GeneID", adjust.method="BH", coef=NULL, directory=NULL, main=NULL) {
  if (is.null(directory)) {
    path <- "plot_data.js"
  } else {
    if (substr(directory, nchar(directory), nchar(directory)) == "/") {
      path <- paste0(directory, "plot_data.js")
    } else {
      path <- paste0(directory, "/plot_data.js")
    }
  }

  if (is.null(main)) {
    main <- "\"\""
  } else {
    main <- paste("\"", main, "\"")
  }

  maJson <- makeMAJson(MArrayLM, genes=genes, p.value=p.value, coef=coef, adjust.method=adjust.method)
  saJson <- makeSAJson(MArrayLM)
  dotJson <- makeDotJson(Elist, sample.groups, labels)
  factJson <- makeFactJson(sample.groups)
  baseURL <- quotify(baseURL)
  linkBy <- quotify(linkBy)
  searchBy <- quotify(searchBy)
  printJsonToFile(c(maJson, dotJson, saJson, factJson, main, baseURL, linkBy, searchBy), path,
                  c("dataMA", "dataDot", "dataSA", "dataFact", "pageTitle", "baseURL", "linkBy", "searchBy"))
}

# Function to write report
interactiveMDplot <- function(object, y, groups, genes=NULL, p.value=0.05, lfc=0, adjust.method="BH",
                         labels=NULL, coef=NULL, baseURL="http://www.ncbi.nlm.nih.gov/gene/?term=", 
                         searchBy="Symbol", linkBy="GeneID", 
                         directory=NULL, launch=TRUE, main=NULL) {
  if (is.null(coef)) {
    stop("coef argument must be specified")
  }

  if (!is(groups, "factor") && !is(groups, "character")) {
    stop("groups arugment must be character or factor vector")
  }

  if (is.null(genes) && is.na(match("genes", names(object)))) {
    stop("please provide gene annotations in genes argument or as attribute of object")
  }

  page.path <- system.file("report_page", package="Glimma")
  files <- paste(page.path, c("plots.html", "plot_utils.js", "plot_styles.css", "utilities.js", "js", "css"), sep="/")

  if (is.null(directory)) {
    wd <- getwd()
    report.path <- "report_page"
  } else {
    report.path <- directory  
  }
  
  dir.create(directory, showWarnings=FALSE)
  
  file.copy(from=files, to=report.path, recursive=TRUE)

  createMAJson(object, y, sample.groups=groups, genes=genes, labels=labels, p.value=p.value, 
                baseURL=baseURL, searchBy=searchBy, linkBy=linkBy,
                adjust.method=adjust.method, coef=coef, directory=report.path, main=main)

  # Launch web page if requested
  if (launch) {
    browseURL(paste(report.path, "/plots.html", sep=""))
  }
}
