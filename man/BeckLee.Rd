\name{BeckLee}
\alias{becklee}

\title{Example dataset for the dispRity package}
\description{
This data set contains a subsample of data from Beck & Lee 2014.
}
\usage{
data(BeckLee)
}

\format{
Contains a phylogeny, two ordinated matrices and a data.frame:
    \describe{
        \item{BeckLee_tree}{A \code{phylo} object contains 50 mammal species.}
        \item{BeckLee_mat50}{A \code{matrix} object; the ordination of the data morphological data from the 50 mammal species.}
        \item{BeckLee_mat99}{A \code{matrix} object; the ordination of the data morphological data from the 50 mammal species and the 49 nodes.}
        \item{BeckLee_ages}{A \code{data.frame} object containing the first and last occurrence data of several mammal species.}
    }
}

\references{Beck RMD & Lee MSY. 2014. Ancient dates or accelerated rates? Morphological clocks and the antiquity of placental mammals. Proc. R. Soc. B 2014 281 20141278; DOI: 10.1098/rspb.2014.1278}
