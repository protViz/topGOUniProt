% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getHumanData.R
\name{getHumanData}
\alias{getHumanData}
\title{gets the human data annotations downloaded from uniprot}
\usage{
getHumanData(PE = 1)
}
\arguments{
\item{PE}{one of 0 = all, 1 = "Evidence at protein level", 2= "Evidence at transcript level", 3= "Inferred from homology",4="Predicted" , 5="Uncertain"}
}
\description{
gets the human data annotations downloaded from uniprot
}
\examples{

library(topGOUniProt)
tmp <-getHumanData(PE= 0)
dim(tmp)
unique(tmp$PE)
tmp <-getHumanData(PE= 1)
unique(tmp$PE)
tmp <-getHumanData(PE= 2)
dim(tmp)

}

