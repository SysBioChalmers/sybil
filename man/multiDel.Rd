\name{multiDel}
\alias{multiDel}

\title{
  Parallel Support for sybil
}

\description{
  Parallel computation support for the functions \code{\link{oneGeneDel}},
  \code{\link{doubleGeneDel}}, \code{\link{oneFluxDel}},
  \code{\link{doubleFluxDel}} and \code{\link{fluxVar}}.
}

\usage{
  multiDel(model, nProc = 2, todo = "oneGeneDel", del1 = NA, del2 = NA, ...)
}

\arguments{
  \item{model}{
    An object of class \code{\link{modelorg}}.
  }
  \item{nProc}{
    Number of cores (processes) to use.
  }
  \item{todo}{
    A single character value giving the function name, which should be
    parallelised. Can be one of \code{"oneGeneDel"},
    \code{"doubleGeneDel"}, \code{"oneFluxDel"},
    \code{"doubleFluxDel"} or \code{"fluxVar"}.
  }
  \item{del1}{
    Vector of genes/reactions to consider.
  }
  \item{del2}{
    Vector of genes/reactions to consider (for use with
    \code{\link{doubleGeneDel}} or \code{\link{doubleFluxDel}}).
  }
  \item{\dots}{
    Further arguments passed to \code{\link{oneGeneDel}},
  \code{\link{doubleGeneDel}}, \code{\link{oneFluxDel}},
  \code{\link{doubleFluxDel}} or \code{\link{fluxVar}}.
  }
}

\details{
  The function loads the package \pkg{parallel} if available.  Argument
  \code{nProc} should be the number of cores to use.  This number is veryfied
  via a call to \code{detectCores} (of \pkg{parallel}) and is set to the return
  value of \code{detectCores}, if \code{nProc > detectCores()} evaluates to
  \code{TRUE}.  Arguments \code{del1} and \code{del2} are split into lists,
  each list element containing \code{nProc/del1} elements. These are passed to
  \code{\link{mclapply}}.
}

\value{
  A list of length \code{nProc} (or less, depending of the numbers of available
  cores), each element containing the return value of the function called (on
  object of a class extending \code{\linkS4class{optsol}}).
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  \code{\link{mclapply}}, \code{\linkS4class{optsol}}, \code{\link{oneGeneDel}},
  \code{\link{doubleGeneDel}}, \code{\link{oneFluxDel}},
  \code{\link{doubleFluxDel}} and \code{\link{fluxVar}}.
}

\examples{
\dontrun{
## The examples here require the packages glpkAPI and parallel to be
## installed.

## perform single gene deletion analysis using the E. coli core
## metabolic model
data(Ec_core)
ad <- multiDel(Ec_core)
mapply(checkOptSol, ad)
}
}
