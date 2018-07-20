\name{checkOptSol-methods}

\docType{methods}
\encoding{utf8}

\alias{checkOptSol}
\alias{checkOptSol-methods}
\alias{checkOptSol,optsol-method}

\title{Summarized Information About an Object of Class Optsol}

\description{
  The function \code{checkOptSol} evaluates the results of the solution
  of optimizations; the returned objects e.g. from \code{\link{optimizeProb}}.
}

\usage{
\S4method{checkOptSol}{optsol}(opt, onlywarn = FALSE)
}

\section{Methods}{
  \describe{
    \item{\code{signature(opt = "optsol")}}{
      method to use with objects of class \code{\linkS4class{optsol}}.
    }
  }
}

\arguments{
  \item{opt}{
    An object of class \code{\linkS4class{optsol}}.
  }
  \item{onlywarn}{
    A single Boolean value. If set to \code{TRUE}, the method will check,
    if all optimizations ended successfully.\cr
    Default: \code{FALSE}.
  }
}

\details{
  The function \code{checkOptSol} is used by functions performing a
  linear optimization (e.g. \code{\link{optimizeProb}}). In that case, the
  argument \code{onlywarn} is set to TRUE. If the optimization ends
  unsuccesfull, a warning will be produced.

  It is also possible to use the function directly, with \code{onlywarn}
  set to FALSE (the default). In that case, an object of class
  \code{\linkS4class{checksol}} will be retuned. This object contains a summary
  with the exit status of the optimization.
}

\value{
  TRUE or FALSE if \code{onlywarn} is set to TRUE, otherwise an object of class
  \code{\linkS4class{checksol}}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\linkS4class{checksol}},
  \code{\link{optimizeProb}} and \code{\link{oneGeneDel}}
}

\examples{
  data(Ec_core)
  Ec_f <- optimizeProb(Ec_core, retOptSol = TRUE)
  Ec_check <- checkOptSol(Ec_f)
}

\keyword{optimize}
