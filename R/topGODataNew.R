#' create an topGodata object
#'
#' @export
#' @examples
#'
#' tmp <-uniprotTable2ProteinGOTable(ontology="CC",mapping=getMouseData())
#' head(tmp)
#'
#' protID <- unique(tmp[1:10,1])
#'
#' protIDbackground <- unique(tmp[1:200,1])
#' res <- topGODataNew(selectedSet = protID , backgroundSet = protIDbackground)
#'
#' summary(res)
#'

topGODataNew <- function( selectedSet , backgroundSet,
                          ontology = "BP",
                          description="Simple session" ,
                          nodeSize = 5, mapping = getMouseData()  )
{
  require(topGO)
  selectfun <- function(x){
    return(x == 1)
  }
  tmp <- as.numeric(backgroundSet %in% selectedSet)
  names(tmp) <- backgroundSet
  sampleGOdata <- new("topGOdata",
                      description = description,
                      ontology = ontology,
                      allGenes = (tmp),
                      geneSelectionFun = selectfun,
                      nodeSize = nodeSize,
                      annotationFun = annFUN.uniprot,
                      mapping = getMouseData())
}

