#' Convert uniprot download to table protein entry and go column
#' @export
#' @examples
#'
#' library(topGOUniProt)
#' tmp <- uniprotTable2ProteinGOTable(ontology="GO",mapping=getMouseData())
#' head(tmp)
#' pids <- unique(tmp[,"Entry"] )
#' res <- getEnrichmentTable(pids[1:100], pids[1:1000] , topNodes = 15, ontology = "BP")
#'
#' tmp <- uniprotTable2ProteinGOTable(ontology="GO",mapping=getHumanData())
#' head(tmp)
#' pids <- unique(tmp[,"Entry"] )
#' res <- getEnrichmentTable(pids[sample(length(pids),2000)], pids , topNodes = 15, ontology = "CC")
#' res
getEnrichmentTable<-function(enrich, background, topNodes=15, ontology="BP"){
  godata <-topGODataNew(enrich,background,ontology = ontology)
  resultFisher <- runTest(godata, algorithm = "classic", statistic = "fisher")
  resultKS <- runTest(godata, algorithm = "classic", statistic = "ks")
  resultKS.elim <- runTest(godata, algorithm = "elim", statistic = "ks")
  allRes <- GenTable(godata, classicFisher = resultFisher,
                     classicKS = resultKS, elimKS = resultKS.elim,
                     orderBy = "elimKS", ranksOf = "classicFisher", topNodes = topNodes)
  return(allRes)
}
