\name{fluxDistribution-class}

\Rdversion{1.1}
\docType{class}

\alias{fluxDistribution-class}
\alias{fluxDistribution}

\alias{[,fluxDistribution,ANY,ANY,ANY-method}
\alias{fluxes,fluxDistribution-method}
\alias{fluxes<-,fluxDistribution-method}
\alias{num_of_fluxes,fluxDistribution-method}
\alias{plot,fluxDistribution,missing-method}
\alias{nnzero,fluxDistribution-method}
\alias{nvar,fluxDistribution-method}
\alias{nvar}

\alias{num_of_fluxes}

\title{Class \code{"fluxDistribution"}}

\description{
  Structure of the class \code{"fluxDistribution"}. Objects of that class are
  used by class \code{"\linkS4class{optsol}"} in order to store flux
  distributions. Flux distributions are stored column by column; each flux
  corresponds to one row and the optimizations correspond to the columns.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{test <- fluxDistribution(fluxes, nrow = 1, ncol = 1}.
  
  If argument \code{fluxes} is of class \code{Matrix} or \code{matrix},
  \code{num_of_fluxes} is set to \code{ncol(fluxes) * nrow(fluxes)}.
  If argument \code{fluxes} is a vector, a matrix will be generated according
  to \code{nrow} and \code{ncol}.
}

\section{Slots}{
  \describe{
    \item{\code{fluxes}:}{
      Object of class \code{"Matrix"} containing fluxdistributions column by
      column.
    }
    \item{\code{num_of_fluxes}:}{
      Object of class \code{"integer"} containing the number of elements in
      \code{fluxes}.
    }
  }
}

\section{Methods}{
  \describe{
    \item{\code{[}}{
      \code{signature(x = "fluxDistribution")}: subsetting operator for the
      matrix of flux distributions.
    }
    \item{\code{fluxes}}{
      \code{signature(object = "fluxDistribution")}: gets the \code{fluxes}
      slot.
    }
    \item{\code{fluxes<-}}{
      \code{signature(object = "fluxDistribution")}: sets the \code{fluxes}
      slot.
    }
    \item{\code{num_of_fluxes}}{
      \code{signature(object = "fluxDistribution")}: gets the
      \code{num_of_fluxes} slot.
    }
    \item{\code{nnzero}}{
      \code{signature(object = "fluxDistribution")}: gets the number of
      non-zero elements in slot \code{fluxes}.
    }
    \item{\code{nvar}}{
      \code{signature(object = "fluxDistribution")}: gets the number of
      fluxes in the fluxdistribution in slot \code{fluxes} (the number of rows
      of slot \code{fluxes}).
    }
    \item{\code{plot}}{
      \code{signature(x = "fluxDistribution", y = "missing")}: heatmap like
      plotting method for fluxdistributions. Not finished yet.
    }
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\examples{
  showClass("fluxDistribution")
}
\keyword{classes}
