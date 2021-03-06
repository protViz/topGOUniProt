% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topGODataNew.R
\name{topGODataNew}
\alias{topGODataNew}
\title{create an topGodata object}
\usage{
topGODataNew(selectedSet, backgroundSet, mapping, ontology = "BP",
  description = "Simple session", nodeSize = 5)
}
\arguments{
\item{selectedSet}{selectedSet}

\item{backgroundSet}{backgroundSet}

\item{mapping}{mapping}

\item{ontology}{ontology to use}

\item{description}{description}

\item{nodeSize}{nodeSize}
}
\description{
create an topGodata object
}
\examples{
library(topGOUniProt)
tmp <-uniprotTable2ProteinGOTable(ontology="CC",mapping=getMouseData(PE=1))
head(tmp)

protID <- unique(tmp[,1])

protIDbackground <- unique(tmp[1:200,1])
res <- topGODataNew(selectedSet = protID , backgroundSet = protIDbackground, mapping=getMouseData(PE=1),  ontology="BP" )
res <- topGODataNew(selectedSet = protID , backgroundSet = protIDbackground,  mapping=getMouseData(PE=1) , ontology="MF")
res <- topGODataNew(selectedSet = protID , backgroundSet = protIDbackground,  mapping=getMouseData(PE=1) , ontology="CC")

tmp <- getHumanData(PE=1)
head(tmp)
protIDbackground <- unique(tmp[,"Entry"])
length(protIDbackground)
protID <- protIDbackground[1:100]

res <- topGODataNew(selectedSet = protID , backgroundSet = protIDbackground,  mapping=getHumanData(PE=1) , ontology="BP")
res <- topGODataNew(selectedSet = protID , backgroundSet = protIDbackground,  mapping=getHumanData(PE=0) , ontology="MF")
res <- topGODataNew(selectedSet = protID , backgroundSet = protIDbackground,  mapping=getHumanData(PE=0) , ontology="CC")

summary(res)

}

