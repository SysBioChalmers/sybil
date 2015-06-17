\name{makeOptsolMO}
\alias{makeOptsolMO}

\title{
  Constructor Function for Objects of Class \code{optsol_optimizeProb}.
}

\description{
  This function is a constructor function generating objects of
  class \code{\linkS4class{optsol_optimizeProb}}.
}

\usage{
  makeOptsolMO(mod, sol)
}

\arguments{
  \item{mod}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{sol}{
      A list returned by function \code{\link{optimizer}}.
  }
}

\value{
  An object of class \code{\linkS4class{optsol_optimizeProb}}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  Class \code{\linkS4class{optsol_optimizeProb}},
  class \code{\linkS4class{modelorg}} and function \code{\link{optimizer}}.
}
