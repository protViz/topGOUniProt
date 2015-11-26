#' topGo compatible annotation funciton
#' @export
#' @examples
#' tmp <-uniprotTable2ProteinGOTable(ontology="CC",mapping=getMouseData())
#' dim(tmp)
#' head(tmp)
#'
#' protID <- unique(tmp[1:10,1])
#' annFUN.uniprot("BP",protID, getMouseData() )
#'
annFUN.uniprot <- function( whichOnto , feasibleGenes , mapping =getMouseData()  ){
  whichOnto = "BP"
  feasibleGenes = protID
  mapping = getMouseData()
  message("ontology: " , whichOnto, " colnames ", colnames(mapping) )
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
