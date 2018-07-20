\name{doubleReact}
\alias{doubleReact}

\encoding{utf8}

\title{Identifies Identical Reactions}

\description{
  The function \code{doubleReact} identifies identical reactions
  (isoenzymes) in a model.
}

\usage{
  doubleReact(model, checkRev = TRUE, linInd = FALSE)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{checkRev}{
    A single logical value. If set to \code{TRUE}, two reactions are identical,
    if, additionally to the stoichiometric coefficients, the direction of the
    reactions is the same (the corresponding value of slot
    \code{\link{react_rev}} of the model).\cr
    Default: \code{TRUE}.
  }
  \item{linInd}{
    A single logical value. If set to \code{TRUE}, two reactions are identical,
    if the vectors of stoichiometric coefficients are linear dependent.
    For example, two reactions with coefficients \eqn{(1, 1, -1)} and
    \eqn{(2, 2, -2)} are linear dependent. If the coefficients have different
    signs, for example \eqn{(-1, 1)} and \eqn{(1, -1)} (the first reaction
    being forward direction and the second one being backward direction), they
    are not identical. If \code{linInd} is set to \code{FALSE}, the
    stoichiometric must be identical, for two reactions considered to be
    identical.
    Default: \code{FALSE}.
  }
}

\details{
  In the first step, the stoichiometric matrix S is divided into
  groups of reactions containing the same number of metabolites. After
  that, the row indices of the non-zero elements of these matrices are
  compared. If identical pairs are found, we check the corresponding
  values in S. If they are also identical, the reversibility of the reactions
  are examined. If they are the same, the two reactions are called identical.
}

\value{
  If no identical reactions were found, the return value is
  FALSE. Otherwise a list is returned, ordered by the number of
  metabolites used in each reaction. Each element is a numerical vector
  containing the indices (column number fo the stoichiometrix matrix) of
  identical reactions.
}


\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\note{
  At the moment, the directions of a pair of reactions is not
  compared. Meaning, that if concerning to the values in S the reaction
  is in forward direction, but not when including the flux values,
  \code{doubleReact} will not find it.
}

\examples{
  data(Ec_core)
  Ec_dr <- doubleReact(Ec_core)
}

\keyword{manip}
