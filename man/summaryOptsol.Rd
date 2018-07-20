\name{summaryOptsol}
\alias{summaryOptsol}

\title{
  Summarize Objects of Class Optsol
}

\description{
  Generates a quick overview of results of simulations stored in objects
  of class \code{\linkS4class{optsol}}.
}

\usage{
  summaryOptsol(opt, mod, perc = 1, tol = SYBIL_SETTINGS("TOLERANCE"))
}

\arguments{
  \item{opt}{
    An object of class \code{\linkS4class{optsol}}.
  }
  \item{mod}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{perc}{
    A single numeric value in between zero and one indicating how close a flux
    value has to reach a flux boundary in order to be called \dQuote{limiting},
    see Details below.\cr
    Default: \code{1}.
  }
  \item{tol}{
    A tolerance value, see Details below.\cr
    Default: \code{SYBIL_SETTINGS("TOLERANCE")}.
  }
}

\details{
  The function \code{summaryOptsol} generates a summary of the simulations
  resulting in the object given in argument \code{opt}. Both model id's, of
  the \code{\linkS4class{optsol}} object and of the
  \code{\linkS4class{modelorg}} object must be identical. The resulting object
  of class \code{\linkS4class{summaryOptsol}} contains information about the
  number of zeros and non-zeros in the flux distribution, the substrates and
  products and about the limiting reactions.
  
  A reaction \eqn{i} is called \dQuote{limiting}, if its flux value \eqn{v_i}
  is non-zero: \eqn{|v_i| > \mathit{tol}}{abs(v_i) > tol} and if its flux value
  hits the flux boundaries:
  \eqn{v_i \leq v_{i,\textrm{min}} \cdot \mathit{perc} \lor
       v_i \geq v_{i,\textrm{max}} \cdot \mathit{perc}}{v_i <= v_i,min * perc
       || v_i >= v_i,max * perc}.
}

\value{
  An object of class \code{\linkS4class{summaryOptsol}} if a flux distribution
  exists in argument \code{opt}, otherwise a \code{\link{summary}} of the
  objective values (\code{\link{mod_obj}}) is returned.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  Class \code{\linkS4class{optsol}}, class \code{\linkS4class{modelorg}} and
  class \code{\linkS4class{summaryOptsol}}.
}

\keyword{classes}
