\name{doubleFluxDel}
\alias{doubleFluxDel}

\encoding{utf8}

\title{Double Flux Deletion Experiment}

\description{
  Double reaction (flux) deletion analysis.
}

\usage{
  doubleFluxDel(model, react1, react2, lb = NULL, ub = NULL,
                allComb = FALSE, exex = FALSE, checkOptSolObj = FALSE, ...)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{react1}{
    An object of class \code{\linkS4class{reactId}} or character or integer
    containing reaction id's to constrain to zero.\cr
    Default: \code{react_id(model)}.
  }
  \item{react2}{
    An object of class \code{\linkS4class{reactId}} or character or integer
    containing reaction id's to constrain to zero.\cr
    Default: \code{react_id(model)}.
  }
  \item{lb}{
    A numeric vector containing the lower bounds for the reaction rates of
    reactions (variables) given in arguments \code{react1} and \code{react2}.
    If set to \code{NULL}, all reactions will be constrained to zero.\cr
    Default: \code{NULL}.
  }
  \item{ub}{
    A numeric vector containing the upper bounds for the reaction rates of
    reactions (variables) given in arguments \code{react1} and \code{react2}.
    If set to \code{NULL}, all reactions will be constrained to zero.\cr
    Default: \code{NULL}.
  }
  \item{allComb}{
    A single Boolean value. If set to \code{TRUE}, every possible pairwise
    combination of reactions given in arguments \code{react1} and \code{react2}
    will be constrained to zero flux. If set to \code{FALSE}, arguments
    \code{react1} and \code{react2} must have the same length. The deletions
    will be computed pair-wise: first \code{react1[1]} and \code{react2[1]},
    second \code{react1[2]} and \code{react2[2]} and so on.\cr
    Default: \code{FALSE}.
  }
  \item{exex}{
    A single Boolean value. If set to \code{TRUE}, exchange reactions will be
    excluded from the analysis. They are identified by the function
    \code{\link{findExchReact}}.
    Default: \code{FALSE}.
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
  The function \code{doubleFluxDel} studies the effect of double flux deletions
  on the phenotype of the metabolic network. The function performs \eqn{n}
  optimizations with \eqn{n} being either the number of reaction id's in
  argument \code{react1} times the number of reaction id's in argument
  \code{react2}, if argument \code{allComb} is set to \code{TRUE}, or the length
  of one of these vectors if argument \code{allComb} is set to \code{FALSE}.
  Each optimization corresponds to the simultaneous deletion of two fluxes.
}

\value{
  An object of class \code{\linkS4class{optsol_fluxdel}}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  \code{\link{modelorg}}, \code{\linkS4class{optsol}},
  \code{\linkS4class{optsol_fluxdel}},
  \code{\link{checkOptSol}}, \code{\link{optimizer}} and
  \code{\link{SYBIL_SETTINGS}}.
}

\examples{
  data(Ec_core)
  Ec_dfd <- doubleFluxDel(Ec_core)
}

\keyword{optimize}

