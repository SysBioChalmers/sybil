\name{checkAlgorithm}
\alias{checkAlgorithm}

\title{
  Check Algorithm
}

\description{
  Test, if a given algorithm can has a certain purpose.
}

\usage{
  checkAlgorithm(alg, purpose)
}

\arguments{
  \item{alg}{
    A single character string containing the name of the algorithm.
  }
  \item{purpose}{
    Purpose of the new algorithm.
  }
}

\value{
  Returns \code{TRUE} if successful, otherwise \code{FALSE}.
}

\author{
  Gabriel Gelius-Dietrich
}

\seealso{
  \code{\link{addAlgorithm}}, \code{\link{getsybilenv}}
}

