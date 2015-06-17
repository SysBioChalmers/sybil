\name{sysBiolAlg}
\alias{sysBiolAlg}

\title{
  General Constructor Function For Objects of Class \code{sysBiolAlg}
}

\description{
  This function serves as a user constructor function for objects of class
  \code{\linkS4class{sysBiolAlg}}.
}

\usage{
  sysBiolAlg(model,
             algorithm = SYBIL_SETTINGS("ALGORITHM"),
             prefix = "sysBiolAlg", sep = "_",
             ...)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{algorithm}{
    A single character string giving the name of the algorithm to use. See
    parameter \code{"ALGORITHM"} in \code{\link{SYBIL_SETTINGS}} for possible
    values.\cr
    Default: \code{SYBIL_SETTINGS("ALGORITHM")}.
  }
  \item{prefix}{
    A single character string containing a prefix for the new class name.\cr
    Default: \code{"sysBiolAlg"}.
  }
  \item{sep}{
    A single character string containing a separator for \code{prefix} and
    \code{algorithm}.\cr
    Default: \code{"_"}.
  }
  \item{\dots}{
    Further arguments passed to the initialize method depending on the
    desired algorithm (see Details below).
  }
}

\details{
  If argument \code{algorithm} is set to \code{"foo"} and \code{prefix} is set
  to \code{"sysBiolAlg"} (default), \code{sysBiolAlg} will try to build an
  instance of class \code{sysBiolAlg_foo}.  If no such class definition exists,
  an error will be returned. For the name of the class, the values of arguments
  \code{prefix} and \code{algorithm} are stick together separated by the value
  of argument \code{sep}: \code{prefix_algorithm}.
  
  Additional arguments required by the initialize method are for example
  \code{solver}, \code{method} and \code{solverParm}.
}

\value{
  An instance of a subclass of
  class \code{\linkS4class{sysBiolAlg}}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  Class \code{\linkS4class{sysBiolAlg}}
}

\examples{
\dontrun{
## The examples here require the package glpkAPI to be
## installed. If that package is not available, you have to set
## the argument 'solver' (the default is: solver = SYBIL_SETTINGS("SOLVER")).

data(Ec_core)

## algorithm: fba (flux balance analysis)
fb <- sysBiolAlg(Ec_core, algorithm = "fba")

## algorithm: lmoma (linearized version of MOMA)
fb <- sysBiolAlg(Ec_core, algorithm = "lmoma")
}
}

\keyword{classes}
