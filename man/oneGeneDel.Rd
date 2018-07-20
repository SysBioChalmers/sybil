\name{oneGeneDel}
\alias{oneGeneDel}

\encoding{utf8}

\title{Single Gene Deletion Experiment}

\description{
  Predict the metabolic phenotype of single-gene knock out mutants.
}

\usage{
  oneGeneDel(model, geneList,
             lb = rep(0, length(geneList)),
             ub = rep(0, length(geneList)),
             checkOptSolObj = FALSE, ...)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{geneList}{
    A character vector containing the set of genes to be deleted one by one.\cr
    Default: \code{allGenes(model)}.
  }
  \item{lb}{
    A numeric vector of the same length as \code{geneList} containing the lower
    bounds for the reaction rates of reactions (variables) affected by the genes
    given in argument \code{geneList}.\cr
    Default: \code{0} for all genes in \code{geneList}, simulating knock-out
    mutants.
  }
  \item{ub}{
    A numeric vector of the same length as \code{geneList} containing the upper
    bounds for the reaction rates of reactions (variables) affected by the genes
    given in argument \code{geneList}.\cr
    Default: \code{0} for all genes in \code{geneList}, simulating knock-out
    mutants.
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
  The function \code{oneGeneDel} studies the effect of genetic perturbations
  by single gene deletions on the phenotype of the metabolic network. The
  function performs \eqn{n} optimizations with \eqn{n} being the length of the
  character vector in argument \code{geneList}. For each gene deletion \eqn{j}
  the set of fluxes effected by the deletion of gene given in \code{geneList[j]}
  is constrained to zero flux. If the deletion of a certain gene has an effect,
  is tested with the function \code{\link{geneDel}}. Each optimization
  corresponds to the deletion of one gene.
}

\value{
  An object of class \code{\linkS4class{optsol_genedel}}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\link{modelorg}}, \code{\linkS4class{optsol}},
  \code{\linkS4class{optsol_genedel}},
  \code{\link{checkOptSol}}, \code{\link{optimizer}} and
  \code{\link{SYBIL_SETTINGS}}.
}

\examples{
  # load example data set
  data(Ec_core)
  
  # compute phenotypes of genetic perturbations via
  # FBA (default)
  Ec_ogd <- oneGeneDel(Ec_core)
  
  # or MOMA (linearized version)
  Ec_ogd <- oneGeneDel(Ec_core, algorithm = "lmoma")
}

\keyword{optimize}

