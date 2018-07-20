\name{addSolver}
\alias{addSolver}

\title{
  Add a New Mathematical Programming Solver to sybil
}

\description{
  Make a new mathematical programming solver available to sybil via the
  \code{\link{SYBIL_SETTINGS}} command.
}

\usage{
  addSolver(solver, method, probType)
}

\arguments{
  \item{solver}{
    A single character string giving the name of the desiered solver.
  }
  \item{method}{
    A character vector of algorithms supported by the solver given in
    \code{solver}.
  }
  \item{probType}{
    A list of hte same length as \code{method} containing a vector of character
    strings for each method which types of problems can be solved with that
    method: \code{method[i]} of \code{solver} can solve problems of type
    \code{probType[[i]]}. Problem types could be \code{"lp"}: linear
    programming, \code{"mip"}: mixed integer programming or \code{"qp"}:
    quadratic programming.
  }
}

\details{
  The parameters to the algorithms given in \code{method} are set to \code{NA},
  which means, the default parameters of the solver software will be used. If a
  solver already exists, an error message will be given.
}

\value{
  The function returns \code{NULL} invisibly.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\link{SYBIL_SETTINGS}}
}

