#' Convert uniprot download to table protein entry and go column
#'
#' @param enrich list of proteins
#' @param background background proteins
#' @param topNodes nr top categories
#' @param ontology ontology to use i.e. BB or CC
#' @param mapping  default getHumanData() or getMouseData()
#'
#' @export
#' @examples
#'
#' library(topGOUniProt)
#' tmp <- uniprotTable2ProteinGOTable(mapping=getMouseData(), ontology="GO")
#' head(tmp)
#' pids <- unique(tmp[,"Entry"] )
#' res <- getEnrichmentTable(pids[1:100], pids[1:1000] , mapping = getMouseData(0), topNodes = 15, ontology = "BP")
#' res
#' tmp <- uniprotTable2ProteinGOTable(mapping=getHumanData(),ontology="GO")
#' head(tmp)
#' pids <- unique(tmp[,"Entry"] )
#' res <- getEnrichmentTable(pids[1:2000], pids ,mapping = getMouseData(0), topNodes = 15, ontology = "MF")
#' res
#' res <- getEnrichmentTable(pids[1:2000], pids , getMouseData(0), topNodes = 15, ontology = "CC")
#' res
#'
getEnrichmentTable<-function(enrich, background, mapping, topNodes=15, ontology="BP"){
  godata <-topGODataNew(enrich,background,mapping = mapping, ontology = ontology)
  resultFisher <- runTest(godata, algorithm = "classic", statistic = "fisher")
  resultKS <- runTest(godata, algorithm = "classic", statistic = "ks")
  resultKS.elim <- runTest(godata, algorithm = "elim", statistic = "ks")
  fischer.weighted <- runTest(godata, algorithm = "weight", statistic = "fisher")

  allRes <- GenTable(godata, classicFisher = resultFisher,
                     classicKS = resultKS, elimKS = resultKS.elim, fischerWeighted = fischer.weighted,
                     orderBy = "elimKS", ranksOf = "classicFisher", topNodes = topNodes)
  return(allRes)
}
