\name{mergeReact2Modelorg}

\docType{methods}
\encoding{utf8}

\alias{mergeReact2Modelorg}
\alias{getReaction}
\alias{getReaction,modelorg-method}
\alias{getReaction,modelorg,ANY-method}

\title{
	Functions to subset and merge modelorg objects.
}
\description{
	The function \code{getReaction} can extract singe \code{react} objects from 
	a modelorg object. If those \code{react} objects are saved in a list, they
	can be passed to the function \code{mergeReact2Modelorg} to combine them to 
	one new model.
}
\usage{
mergeReact2Modelorg(reactList = NULL, id = "newModel", name = "")
\S4method{getReaction}{modelorg,ANY}(X, j = NULL, drop = T, tol = SYBIL_SETTINGS("TOLERANCE"))
}
\arguments{
  \item{reactList}{
	list of \code{react} objects
}
  \item{id}{
	id for the new \code{modelorg}
}
  \item{name}{
	name for the new \code{modelorg}
}
\item{j}{
	defines the reaction numbers or IDs to extract from the model.
}
\item{drop}{
	If FALSE, a list of length 1 is returned.
}
\item{tol}{
	Threshold for coefficients to be unequal zero.
}
\item{X}{
	\code{modelorg} object to extract reactions from.
}
}
\value{
	mergeReact2Modelorg returns a \code{modelorg} object.
	
	getReaction returns a \code{react} object if \code{length(j) = 1} and \code{drop = TRUE}. Otherwise
	a list of \code{react} objects.
}
\author{
  Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}

\seealso{
	\code{\link{modelorg}}, \code{\link{react}}
}
\examples{
	data(Ec_core)
	l <- getReaction(Ec_core, j=1:3)
	print(l)
	m <- mergeReact2Modelorg(l)
	print(m)

}
\keyword{ subset }
\keyword{ merge }
