\name{netFlux-class}

\Rdversion{1.1}
\encoding{utf8}
\docType{class}

\alias{netFlux}
\alias{netFlux-class}
\alias{getNetFlux}
\alias{length,netFlux-method}
\alias{rate,netFlux-method}
\alias{rate}
\alias{react_id,netFlux-method}
\alias{react_id<-,netFlux-method}

\title{Class \code{"netFlux"}}

\description{
  Class \code{"netFlux"} groups exchange reaction rates according to their sign
  in uptake, excretion and unused reactions.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{getNetFlux(rates, tol)},
  with argument \code{rates} being a named numeric vector containing reaction
  rates of exchange fluxes and corresponding reaction id's. Argument
  \code{rates} can be obtained by a call to \code{\link{optimizeProb}}. The
  second argument \code{tol} is a tolerance value (default:
  \code{SYBIL_SETTINGS("TOLERANCE")}). Reaction rates less than \code{tol * -1}
  are uptake reactions, reaction rates greater than \code{tol} are excretion
  reactions and all others (\code{abs(rates) < tol}) are unused reactions.
}

\section{Slots}{
  \describe{
    \item{\code{uptake}:}{
      Object of class \code{"logical"} indicating uptake reactions.
    }
    \item{\code{product}:}{
      Object of class \code{"logical"} indicating excretion reactions.
    }
    \item{\code{unused}:}{
      Object of class \code{"logical"} indicating unused reactions.
    }
    \item{\code{react_id}:}{
      Object of class \code{"character"} containing the reaction id's of the
      exchange reactions.
    }
    \item{\code{rate}:}{
      Object of class \code{"numeric"} containing the reaction rates of the
      exchange reactions.
    }
  }
}

\section{Methods}{
  \describe{
    \item{length}{\code{signature(x = "netFlux")}:
      number of exchange reactions.
    }
    \item{rate}{\code{signature(object = "netFlux")}:
      gets the \code{rate} slot.
    }
    \item{react_id}{\code{signature(object = "netFlux")}:
      gets the \code{react_id} slot.
    }
    \item{react_id<-}{\code{signature(object = "netFlux")}:
      sets the \code{react_id} slot.
    }
  }
}

\author{Gabriel Gelius-Dietrich}

\seealso{
  \code{\link{optimizeProb}}, \code{\link{getFluxDist}}
}

\examples{
  data(Ec_core)
  # retrieve all exchange reactions
  ex  <- findExchReact(Ec_core)
  # perform flux balance analysis
  opt <- optimizeProb(Ec_core, algorithm = "fba")
  # get flux distribution of all exchange reactions
  fd  <- getFluxDist(opt, ex)
  # group exchange reactions
  getNetFlux(fd)
}

\keyword{classes}
