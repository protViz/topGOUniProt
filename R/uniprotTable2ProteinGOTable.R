#' Convert uniprot download to table protein entry and go column
#' @export
#' @examples
#'
#' tmp <-uniprotTable2ProteinGOTable(ontology="GO",mapping=getMouseData())
#' dim(tmp)
#' head(tmp)
#' tmp <-uniprotTable2ProteinGOTable(ontology="BP",mapping=getMouseData())
#' dim(tmp)
#' head(tmp)
#' tmp <-uniprotTable2ProteinGOTable(ontology="CC",mapping=getMouseData())
#' dim(tmp)
#' head(tmp)
#'

uniprotTable2ProteinGOTable <- function(ontology = "GO", mapping = getMouseData()){
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  GO <- mapping[[ontology]]
  names(GO) <- mapping$Entry
  GO2<-lapply(GO, function(x){x <- strsplit(x, ";")[[1]]; x <- trim(x); return(x)})

  ## createTable where first column
  ## is the protein id and the second one is the go id.
  res <- vector(mode="list", length=length(GO2))
  for( i in 1:length(GO2)){
    if(length(GO2[[i]])==0){
      #print("xxxxx ")
    }else{
      tmp <- cbind(names(GO2)[i], GO2[[i]])
      res[[i]] <- tmp
    }
  }
  mouseUniProt <- do.call("rbind",res)
  colnames(mouseUniProt) <- c("Entry",ontology)
  mouseUniProt[,2]<-gsub(".*\\[(.*)\\]","\\1",mouseUniProt[,2])
  return(mouseUniProt)
}
