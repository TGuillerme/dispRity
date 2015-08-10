\name{tree.age}
\alias{tree.age}

\title{Calculating the age of nodes and tips in a tree.}

\description{Calculates the age of each nodes and tips in a tree give the hight of the tree or some specified age.}

\usage{
tree.age(tree, age, order='past')
}

\arguments{
\item{tree}{a `phylo`' object.}
\item{age}{the age of the tree. If missing the age is set to be the tree hight.}
\item{order}{either 'past' if the units is express time since the present (e.g. Mya), or 'present' if the unit is expressed in time since the root.}

%\details{If needed}

%\seealso{\link{disparity} or any other functions}

\examples{
## A dated random phylogeny with a root 50 units of time old.
tree.age(rtree(10), age=50)
## A random tree with the distance since the root.
tree.age(rtree(10), order='present')
}

\author{Thomas Guillerme}