\name{findExchReact}

\alias{findExchReact}

\encoding{utf8}

\title{
  Find Exchange Reactions
}

\description{
  This function identifies reactions in a metabolic network which transport
  metabolites accross the network boundary. Only the stroichiometric matrix
  is taken into account, so the identified reactions are basically those,
  having only one non-zero entry in their column of the stroichiometric matrix.
  In order to work, the network must be \dQuote{open}, it must not contain
  boundary metabolites.
}

\usage{
  findExchReact(model)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}},
    \code{\link[Matrix]{Matrix}} or \code{\link{matrix}}.
  }
}

\value{
  If \code{model} is of class \code{\linkS4class{modelorg}} an object of class
  \code{\linkS4class{reactId_Exch}} is returned.
  Otherwise, if \code{model} is of class \code{\link{matrix}} or
  of class \code{\link[Matrix]{Matrix}}, a logical vector is returned.
  If element \code{i} equals \code{TRUE}, column \code{i} of \code{model} is an
  exchange reaction. The function returns \code{\link{NULL}} and gives a
  warning, if no exchange reaction can be found. 
}

\details{
  A exchange reaction \eqn{j} for a particular metabolite \eqn{i} has exactly
  one non-zero entry in the stoichiometric matrix
  \eqn{S_{ij} \in \{-1, 1\}}{S_ij \in {-1, 1}}. If \eqn{S_{ij} = -1}{S_ij = -1},
  reaction \eqn{j} is considered to be an uptake (source) reaction.
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

\examples{
  data(Ec_core)
  ex  <- findExchReact(Ec_core)
  
  # run FBA
  opt <- optimizeProb(Ec_core)
  
  # get flux distribution of exchange reactions
  getFluxDist(opt, ex)
}

\author{Gabriel Gelius-Dietrich}

