\name{checkReactId}

\alias{checkReactId}

\encoding{utf8}

\title{Check if a Reaction Id is Valid}

\description{
  The function \code{checkReactId} evaluates a vector of reaction id's
  if they are unique and appear in a given model.
}

\usage{
  checkReactId(model, react)
}

\arguments{
  \item{model}{A model. An object of class
    \code{\linkS4class{modelorg}}, or a problem object of a lp solver.}
  \item{react}{Character vector containing reaction id's, or a numerical
    vector containing indices of reaction id's.}
}

\details{
  If argument \code{react} is numeric, the maximun value will be
  inspected, if it is larger than the number of reactions in the model.

  In case of a character vector, \code{react} is matched to the reaction
  id's residing in the model. If they are not found, grep is used.

  If argument \code{react} is of class \code{\linkS4class{reactId}}, it will
  be returned without checking.
}

\value{
  An object of class \code{\linkS4class{reactId}} or \code{NULL} if argument
  \code{react} contains any reactions not in \code{model}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{\code{\linkS4class{reactId}}}

\examples{
  data(Ec_core)
 
  ## Example with react as character vector
  ids <- c("ATPM", "ACK")
  idc <- checkReactId(Ec_core, ids)

  ## Example with react as numerical vector
  ids <- c(1:4)
  idc <- checkReactId(Ec_core, ids)
}

\keyword{character}

