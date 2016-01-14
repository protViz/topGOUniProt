#' gets the human data annotations downloaded from uniprot
#'
#' @export
#' @examples
#'
#' library(topGOUniProt)
#' mapping <- getHumanData()
#' head(mapping)
#'
getHumanData <- function() {
  filepath <- file.path( path.package("topGOUniProt") , "extdata/uniprot-homo+sapiens.tab" )
  resID <- read.csv( filepath, sep="\t", header=TRUE, stringsAsFactors = FALSE )
  colnames(resID) <- c("Entry", "GO", "CC","BP", "MF" )
  return(resID)
}

