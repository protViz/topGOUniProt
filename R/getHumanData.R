.getGoTerms<-function(filepath,PE=0){
  evidence <-c("Evidence at protein level","Evidence at transcript level","Inferred from homology","Predicted","Uncertain")
  resID <- read.csv( filepath, sep="\t", header=TRUE, stringsAsFactors = FALSE )
  colnames(resID) <- c("Entry", "GO", "CC","BP", "MF","PE" )
  if(PE == 0){
    return(resID)
  }
  resID <- resID[resID[,"PE"] == evidence[PE], ]
  return(resID)
}
#' gets the human data annotations downloaded from uniprot
#'
#' @param PE one of 0 = all, 1 = "Evidence at protein level", 2= "Evidence at transcript level", 3= "Inferred from homology",4="Predicted" , 5="Uncertain"
#' @export
#' @examples
#'
#' library(topGOUniProt)
#' tmp <-getHumanData(PE= 0)
#' dim(tmp)
#' unique(tmp$PE)
#' tmp <-getHumanData(PE= 1)
#' unique(tmp$PE)
#' tmp <-getHumanData(PE= 2)
#' dim(tmp)
#'
getHumanData <- function( PE=1 ) {
  filepath <- file.path( path.package("topGOUniProt") , "extdata/uniprot-homo+sapiens.tab" )
  res <- .getGoTerms(filepath,PE)
  return(res)
}
#' gets the mouse data annotations downloaded from uniprot
#'
#' @param PE one of 0 = all, 1 = "Evidence at protein level", 2= "Evidence at transcript level", 3= "Inferred from homology",4="Predicted" , 5="Uncertain"
#' @export
#' @examples
#' library(topGOUniProt)
#' tmp <- getMouseData(PE=0)
#' colnames(tmp)
#' unique(tmp$PE)
#' tmp <- getMouseData()
#' unique(tmp$PE)
getMouseData <- function(PE=1){
  filepath <- file.path(path.package("topGOUniProt"),"extdata/uniprot-mus+musculus.tab")
  res <- .getGoTerms(filepath,PE)
  return(res)
}
