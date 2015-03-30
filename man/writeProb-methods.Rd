\name{writeProb-methods}

\docType{methods}
\encoding{utf8}

\alias{writeProb}
\alias{writeProb-methods}

\alias{writeProb,optObj_clpAPI,character-method}
\alias{writeProb,optObj_cplexAPI,character-method}
\alias{writeProb,optObj_glpkAPI,character-method}
\alias{writeProb,optObj_lpSolveAPI,character-method}

\title{Write Problem Object to File}

\description{
  Write problem object to file (e.g. in lp format).
}

\usage{
\S4method{writeProb}{optObj_clpAPI,character}(lp, fname, ff = "lp")

\S4method{writeProb}{optObj_cplexAPI,character}(lp, fname, ff = "lp")

\S4method{writeProb}{optObj_glpkAPI,character}(lp, fname, ff = "lp", ...)

\S4method{writeProb}{optObj_lpSolveAPI,character}(lp, fname, ff = "lp", ...)
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
    A single character string giving the file name to write to.
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
  Argument \code{"ff"} is unused with \pkg{clpAPI}. Valid values for
  \pkg{cplexAPI} and \code{lpSolveAPI} are available in their documentations.
  For \pkg{glpkAPI}, argument \code{"ff"} can be \code{"lp"} for LP file format,
  \code{"mps"} for MPS file format or \code{"glpk"} for GLPK file format.
}

\author{
  Gabriel Gelius-Dietrich
}

\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
  Method to read problem objects: \code{\link{readProb}}
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
