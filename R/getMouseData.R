#' gets the mouse data annotations downloaded from uniprot
#'
#' @export
#' @examples
#' library(topGOUniProt)
#' mapping <- getMouseData()
#' head(mapping)
getMouseData <- function(){
  filepath <- file.path(path.package("topGOUniProt"),"extdata/uniprot-mus+musculus.tab")
  resID <- read.csv(filepath,sep="\t",header=TRUE,stringsAsFactors = FALSE)
  colnames(resID) <- c("Entry","SL", "GO", "CC","BP", "MF" )
  return(resID)
}
