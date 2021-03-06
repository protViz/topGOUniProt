#' topGo compatible annotation function
#'
#' @param whichOnto - which ontology to use i.e. "BP", "CC"
#' @param feasibleGenes feasible Genes
#' @param mapping id go term mappings
#' @export
#' @examples
#' library(topGoUniProt)
#' tmp <-uniprotTable2ProteinGOTable(mapping=getMouseData(), ontology="CC")
#' dim(tmp)
#' head(tmp)
#'
#' protID <- unique(tmp[1:10,1])
#' t1 <- annFUN.uniprot("BP",protID, getMouseData(0) )
#' length(t1)
#' t1<-annFUN.uniprot("CC",protID, getMouseData(0) )
#' length(t1)
#' names(t1)
annFUN.uniprot <- function( whichOnto , feasibleGenes, mapping){
  message("ontology: " , whichOnto, " colnames ", paste(colnames(mapping), collapse=" ") )
  mapping <- uniprotTable2ProteinGOTable(mapping = mapping, ontology = whichOnto )
  matchingProteinSet <- mapping[mapping[,1] %in% feasibleGenes,]
  GOS<-unique(matchingProteinSet[,2])
  res <- list()

  for(GO in GOS){
    #print(matchingProteinSet[matchingProteinSet[,2] %in% GO ,1])
    res[[GO]] <- matchingProteinSet[matchingProteinSet[,2] %in% GO ,1]
  }
  return(res)
}
