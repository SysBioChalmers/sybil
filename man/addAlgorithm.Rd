\name{addAlgorithm}
\alias{addAlgorithm}

\title{
  Add a New Algorithm Name to sybil
}
\description{
  Certain simulations can be run using different algorithms. For example,
  genetic perturbations can be studied with FBA, MOMA or the like. With this
  funktion you can add a new algorithm to an existing kind of simulation.
}
\usage{
  addAlgorithm(alg, purpose)
}

\arguments{
  \item{alg}{
    A single character string containing the name of the new algorithm.
  }
  \item{purpose}{
    Purpose of the new algorithm.
  }
}

\value{
  Returns \code{NULL} invisibly.
}

\author{
  Gabriel Gelius-Dietrich
}

\seealso{
  \code{\link{checkAlgorithm}}, \code{\link{getsybilenv}}
}

