\name{blockedReact}

\alias{blockedReact}

\title{
  Find Blocked Reactions in a Metabolic Network
}

\description{
  A blocked Reaction in a metabolic network can not be used by the network,
  given the stiochiometric matrix of the network and a set of input and output
  fluxes.
}

\usage{
  blockedReact(model,
               tol = SYBIL_SETTINGS("TOLERANCE"),
               exex = TRUE,
               fld = FALSE,
               retOptSol = FALSE,
               verboseMode = 2,
               ...)
}

\arguments{
  \item{model}{
    An object of class \code{\link{modelorg}}.
  }
  \item{tol}{
    Tolerance value.\cr
    Default: \code{SYBIL_SETTINGS("TOLERANCE")}.
  }
  \item{exex}{
    Boolean, if set to \code{TRUE}, exchange reactions found by
    \code{\link{findExchReact}} are excluded from the analysis.\cr
    Default: \code{TRUE}.
  }
  \item{fld}{
    Boolean.  Save the resulting flux distributions.\cr
    Default: \code{FALSE}
  }
  \item{retOptSol}{
    Boolean.  Return an object of class
    \code{\linkS4class{optsol_blockedReact}} or
    just a list containing the results.\cr
    Default: \code{FALSE}.
  }
  \item{verboseMode}{
    An integer value indicating the amount of output to stdout:
    0: nothing, 1: status messages, 2: like 1 plus a progress indicator.\cr
    Default: \code{2}.
  }
  \item{\dots}{
    Further arguments passed to \code{\linkS4class{sysBiolAlg}}.
    Argument \code{solverParm} is a good candidate.
  }
}

\details{
  A reaction \eqn{i} is considered to be \sQuote{blocked}, if its calculated
  reaction rate \eqn{v_i}{v[i]} is
  \eqn{-\mathrm{tol} < v_i < \mathrm{tol}}{-tol < v[i] < tol}.
  Reaction rates are calculated via linear optimization: maximizing and
  minimizing each reaction rate. If the difference of the maximum and the
  minimum is not larger than \code{tol}, that particular reaction is blocked,
  given the current side conditions (exchange fluxes).
}

\value{
  If argument \code{retOptsol} is set to \code{TRUE}, an object of class
  \code{\linkS4class{optsol_blockedReact}} is returned, otherwise a logical
  vector with length equal to the number of reactions of the network. If element
  \code{i} equals \code{TRUE}, reaction \code{i} is blocked.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\link{modelorg}},
  \code{\linkS4class{optsol_blockedReact}} and
  \code{\link{SYBIL_SETTINGS}}.
}

\keyword{optimize}
