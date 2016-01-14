#' create an topGodata object
#'
#' @param selectedSet selectedSet
#' @param backgroundSet backgroundSet
#' @param ontology ontology to use
#' @param description description
#' @param nodeSize nodeSize
#' @param mapping mapping
#' @export
#' @examples
#'
#' tmp <-uniprotTable2ProteinGOTable(ontology="CC",mapping=getMouseData())
#' head(tmp)
#'
#' protID <- unique(tmp[1:10,1])
#'
#' protIDbackground <- unique(tmp[1:200,1])
#' res <- topGODataNew(selectedSet = protID , backgroundSet = protIDbackground, ontology="BP")
#' res <- topGODataNew(selectedSet = protID , backgroundSet = protIDbackground, ontology="CC")
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
  message(ontology , "ontology")
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

