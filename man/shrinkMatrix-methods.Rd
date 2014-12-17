\name{shrinkMatrix-methods}

\docType{methods}
\encoding{utf8}

\alias{shrinkMatrix}
\alias{shrinkMatrix-methods}
\alias{shrinkMatrix,modelorg-method}

\title{Get a Subset of Matrix Like Objects}

\description{
  Generate subsets of matrix-like objects.
}

\usage{
\S4method{shrinkMatrix}{modelorg}(X, i = NULL, j = NULL,
                                  tol = SYBIL_SETTINGS("TOLERANCE"))
}

\section{Methods}{
  \describe{
    \item{\code{signature(X = "modelorg")}}{
      method to use with objects of class \code{\linkS4class{modelorg}} for
      subsets of the stoichiometric matrix. Either argument \code{i} or
      argument \code{j} can be used, not both at the same time. If they are of
      type character, they must contain metabolite or reaction id's existing
      in the \code{\linkS4class{modelorg}} object. Use \code{i} to get the
      reactions in which the metabolites given in \code{i} participate (the
      metabolites given in \code{i} will be located in the first rows of the
      result). Use \code{j} to get all reactions given in \code{j}. The method
      will remove all non-zero rows and columns from the result.
    }
  }
}

\arguments{
  \item{X}{
    An object treated to be matrix-like.
  }
  \item{i}{
    A numeric or character vector containing row indices of the matrix
    given in argument \code{X}. For the \code{\linkS4class{modelorg}} method,
    this can be an object of class \code{\linkS4class{reactId_Exch}}.\cr
    Default: \code{NULL}.
  }
  \item{j}{
    A numeric or character vector containing column indices of the
    matrix given in argument \code{X}. For the \code{\linkS4class{modelorg}}
    method, this can be an object of class \code{\linkS4class{reactId}}.\cr
    Default: \code{NULL}.
  }
  \item{tol}{
    A tolerance value. An element \eqn{X_{ij}}{X[i, j]} of the matrix given in
    argument \code{X} is considered to be zero, if
    \eqn{|X_{ij}| > \mathit{tol}}{abs([X[i, j]]) > tol} is true.\cr
    Default: \code{SYBIL_SETTINGS("TOLERANCE")}.
  }
}

\value{
  The \code{\linkS4class{modelorg}} method will return an object of class
  \code{\linkS4class{Matrix}}, with columns named by their reaction id's and
  rows named by their metabolite id's.
}

\examples{
  # get the part of the stoichiometric containing
  # the exchange reactions
  data(Ec_core)
  ex <- findExchReact(Ec_core)
  shrinkMatrix(Ec_core, j = ex)
}

\author{
  Claus Jonathan Fritzemeier, Gabriel Gelius-Dietrich
}

\seealso{
  Class \code{\linkS4class{modelorg}}.
}

\keyword{methods}
