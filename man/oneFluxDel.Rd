\name{oneFluxDel}
\alias{oneFluxDel}

\encoding{utf8}

\title{Single Flux Deletion Experiment}

\description{
  Single reaction (flux) deletion analysis.
}

\usage{
  oneFluxDel(model, react = c(1:react_num(model)),
             lb = rep(0, length(react)),
             ub = rep(0, length(react)),
             checkOptSolObj = FALSE, ...)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{react}{
    An object of class \code{\linkS4class{reactId}} or character or integer
    containing reaction id's to constrain to zero one by one.\cr
    Default: all reactions present in argument \code{model}.
  }
  \item{lb}{
    A numeric vector of the same length as \code{react} containing the lower
    bounds for the reaction rates of reactions (variables) given in argument
    \code{react}.\cr
    Default: \code{0} for all reactions in \code{react}, zero flux through all
    reactions.
  }
  \item{ub}{
    A numeric vector of the same length as \code{react} containing the lower
    bounds for the reaction rates of reactions (variables) given in argument
    \code{react}.\cr
    Default: \code{0} for all reactions in \code{react}, zero flux through all
    reactions.
  }
  \item{checkOptSolObj}{
    A single logical value. If set to \code{TRUE}, a warning will be generated,
    if not all optimizations ended successful.\cr
    Default: \code{FALSE}.
  }
  \item{\dots}{
    Further arguments passed to \code{\link{optimizer}}.  Important ones are
    \code{algorithm} in order to set the algorithm to use or \code{solverParm}
    in order to set parameter values for the optimization software.
  }
}

\details{
  The function \code{oneFluxDel} studies the effect of constraining single
  fluxes to zero flux rates on the phenotype of the metabolic network. The
  function performs \eqn{n} optimizations with \eqn{n} being the number of
  reaction id's given in argument \code{react}. Each optimization corresponds
  to the removal of one reaction.
}

\value{
  An object of class \code{\linkS4class{optsol_fluxdel}}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\link{modelorg}}, \code{\linkS4class{optsol}},
  \code{\linkS4class{optsol_fluxdel}},
  \code{\link{checkOptSol}}, \code{\link{optimizer}} and
  \code{\link{SYBIL_SETTINGS}}.
}

\examples{
  data(Ec_core)
  Ec_ofd <- oneFluxDel(Ec_core)
}

\keyword{optimize}

