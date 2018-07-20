\name{getsybilenv}
\alias{getsybilenv}

\title{
  Print sybil Environment
}

\description{
  Prints current settings in the sybil environment.
}

\usage{
  getsybilenv(part)
}

\arguments{
  \item{part}{
    A character vector containing names of elements in the sybil environment.
    Possible values are:
    \describe{
      \item{\code{"solvers"}}{
        supported R packages for solving optimization problems.
      }
      \item{\code{"methods"}}{
        methods to solve optimization problems included in the R packages.
      }
      \item{\code{"ptype"}}{
        methods required for a particular problem type.
      }
      \item{\code{"purpose"}}{
        algorithms used in systems biology to use with a particular purpose.
      }
    }
  }
}

\details{
  Typical usages are
  \preformatted{
    getsybilenv(part)
    getsybilenv()
  }
  If argument \code{part} is not given, all elements described above will be
  printed.
}

\value{
  Returns \code{NULL} invisibly.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\link{addSolver}}, \code{\link{checkDefaultMethod}}
  and \code{\link{SYBIL_SETTINGS}}.
}

