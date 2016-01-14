#' topGo compatible annotation function
#'
#' @param whichOnto - which ontology to use i.e. "BP", "CC"
#' @param feasibleGenes feasible Genes
#' @param mapping id go term mappings
#' @export
#' @examples
#' tmp <-uniprotTable2ProteinGOTable(ontology="CC",mapping=getMouseData())
#' dim(tmp)
#' head(tmp)
#'
#' protID <- unique(tmp[1:10,1])
#' annFUN.uniprot("BP",protID, getMouseData() )
#' annFUN.uniprot("CC",protID, getMouseData() )
#'
annFUN.uniprot <- function( whichOnto , feasibleGenes , mapping = getMouseData() ){
  message("ontology: " , whichOnto, " colnames ", paste(colnames(mapping), collapse=" ") )
  mapping <- uniprotTable2ProteinGOTable(ontology = whichOnto, mapping = mapping)
  matchingProteinSet <- mapping[mapping[,1] %in% feasibleGenes,]
  GOS<-unique(matchingProteinSet[,2])
  res <- list()

  for(GO in GOS){
    #print(matchingProteinSet[matchingProteinSet[,2] %in% GO ,1])
    res[[GO]] <- matchingProteinSet[matchingProteinSet[,2] %in% GO ,1]
  }
  return(res)
}
