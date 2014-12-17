\name{addExchReact}
\alias{addExchReact}

\encoding{utf8}

\title{
  Add Exchange Reactions to a Model
}

\description{
  The function \code{addExchReact} adds exchange reactions for a set of
  metabolites to a metabolic model.
}

\usage{
  addExchReact(model, met, lb, ub)
}

\arguments{
  \item{model}{
    An object of class \code{modelorg}.
  }
  \item{met}{
    A vector of character strings containing the metabolite id's to add exchange
    reactions for.
  }
  \item{lb}{
    A vector of numeric values of the same length as \code{met} containing the
    lower bounds for the exchange reactions.\cr
    Default: \code{rep(0, length(met))}.
  }
  \item{ub}{
    A vector of numeric values of the same length as \code{met} containing the
    upper bounds for the exchange reactions.\cr
    Default: \code{rep(SYBIL_SETTINGS("MAXIMUM"), length(met))}.
  }
}

\details{
  If \code{lb[i] < 0}, the exchange reaction for the metabolite in \code{met[i]}
  is considered to be reversible, otherwise irreversible. A reaction id is
  generated for each exchange reaction by prepending the metabolite id's with
  the string \code{"Ex_"}.
}

\value{
  An object of class \code{\linkS4class{modelorg}}
}

\references{
  Becker, S. A., Feist, A. M., Mo, M. L., Hannum, G., Palsson, B. Ø.  and
  Herrgard, M. J. (2007) Quantitative prediction of cellular metabolism with
  constraint-based models: the COBRA Toolbox. \emph{Nat Protoc} \bold{2},
  727--738.

  Schellenberger, J., Que, R., Fleming, R. M. T., Thiele, I., Orth, J. D.,
  Feist, A. M., Zielinski, D. C., Bordbar, A., Lewis, N. E., Rahmanian, S.,
  Kang, J., Hyduke, D. R. and Palsson, B. Ø. (2011) Quantitative prediction of
  cellular metabolism with constraint-based models: the COBRA Toolbox v2.0.
  \emph{Nat Protoc} \bold{6}, 1290--1307.
}

\author{Gabriel Gelius-Dietrich}

\seealso{
  \code{\linkS4class{modelorg}} and \code{\link{addReact}}
}

\examples{
  # add exchange reactions (allowing input) for the metabolites
  # malate and oxalacetate
  data(Ec_core)
  mod <- addExchReact(Ec_core,
                      met = c("mal_L[c]", "oaa[c]"),
                      lb = c(-20, -20))
  findExchReact(mod)
}

\keyword{manip}
