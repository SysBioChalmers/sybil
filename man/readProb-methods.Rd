\name{readProb-methods}

\docType{methods}
\encoding{utf8}

\alias{readProb}
\alias{readProb-methods}

\alias{readProb,optObj_clpAPI,character-method}
\alias{readProb,optObj_cplexAPI,character-method}
\alias{readProb,optObj_glpkAPI,character-method}
\alias{readProb,optObj_lpSolveAPI,character-method}

\title{Read Problem Object From File}

\description{
  Read problem object from file.
}

\usage{
\S4method{readProb}{optObj_clpAPI,character}(lp, fname, ff = "mps", ...)

\S4method{readProb}{optObj_cplexAPI,character}(lp, fname, ff = "lp")

\S4method{readProb}{optObj_glpkAPI,character}(lp, fname, ff = "lp", ...)

\S4method{readProb}{optObj_lpSolveAPI,character}(lp, fname, ff = "lp", ...)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI", fname = "character")}}{
      method to use with package \pkg{optObj_clpAPI}. Argument \code{ff} is
      not used here.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", fname = "character")}}{
      method to use with package \pkg{optObj_cplexAPI}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", fname = "character")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI", fname = "character")}}{
      method to use with package \pkg{optObj_lpSolveAPI}.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{fname}{
    A single character string giving the file name to read from.
  }
  \item{ff}{
    A single character string giving the file format to use, see Details.\cr
    Default: \code{"lp"}.
  }
  \item{...}{
    Further arguments passed to the corresponding API routine.
  }
}

\details{
  Argument \code{"ff"} in conjunction with \pkg{clpAPI} can be \code{mps} for
  MPS file format or \code{"clp"} for COIN-OR Clp file mormat. Valid values for
  \pkg{cplexAPI} and \code{lpSolveAPI} are available in their documentations.
  For \pkg{glpkAPI}, argument \code{"ff"} can be \code{"lp"} for LP file format,
  \code{"mps"} for MPS file format or \code{"glpk"} for GLPK file format.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
  Method to write problem objects: \code{\link{writeProb}}
}

\examples{
	\dontrun{
		# In very rare cases it is handy to save a sysBiolAlg-object:

		library(sybil)
		data(Ec_core)
		prob <- sysBiolAlg(Ec_core, algorithm = "fba", solver="glpkAPI") # create a sysBiolAlg object (we use here GLPK (!))
		save(file="prob.RData",prob) # write the R-object to disc
		writeProb(prob@problem, fname="prob.lp", ff="lp") # now write the linear program part (managed by the solver) to disc

		# start new R session

		library(sybil)
		library(glpkAPI)
		load("prob.RData") # restore the R-object
		prob@problem@oobj <- initProbGLPK() # initialize a new linear program
		readProb(problem(prob), fname="prob.lp") # load the previously saved linear program
	}
}

\keyword{methods}
\keyword{optimize}
