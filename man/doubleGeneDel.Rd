\name{doubleGeneDel}
\alias{doubleGeneDel}

\encoding{utf8}

\title{Double Gene Deletion Experiment}

\description{
  Predict the metabolic phenotype of of double-gene knock out mutants.
}

\usage{
  doubleGeneDel(model, geneList1, geneList2, lb = NULL, ub = NULL,
                allComb = FALSE, exLethal = TRUE,
                tol = SYBIL_SETTINGS("TOLERANCE"),
                checkOptSolObj = FALSE, ...)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{geneList1}{
    A character vector containing the set of genes to be deleted.\cr
    Default: \code{allGenes(model)}.
  }
  \item{geneList2}{
    A character vector containing the set of genes to be deleted.\cr
    Default: \code{allGenes(model)}.
  }
  \item{lb}{
    A numeric vector containing the lower bounds for the reaction rates of
    reactions (variables) affected by the genes given in arguments
    \code{geneList1} and \code{geneList2}. If set to \code{NULL}, all reactions
    affected will be constrained to zero.\cr
    Default: \code{NULL}.
  }
  \item{ub}{
    A numeric vector containing the upper bounds for the reaction rates of
    reactions (variables) affected by the genes given in arguments
    \code{geneList1} and \code{geneList2}. If set to \code{NULL}, all reactions
    affected will be constrained to zero.\cr
    Default: \code{NULL}.
  }
  \item{allComb}{
    A single Boolean value. If set to \code{TRUE}, every possible pairwise
    combination of genes given in arguments \code{geneList1} and
    \code{geneList2} will be knocked-out. If set to \code{FALSE}, arguments
    \code{geneList1} and \code{geneList2} must have the same length. The
    knock-outs will be computed pair-wise: first \code{geneList1[1]} and
    \code{geneList2[1]}, second \code{geneList1[2]} and \code{geneList2[2]}
    and so on.\cr
    Default: \code{FALSE}.
  }
  \item{exLethal}{
    A single Boolean value. If set to \code{TRUE}, lethal genes are removed from
    the analysis. A unique set of genes in \code{geneList1} and \code{geneList2}
    will be scanned for lethal genes. A particular gene \eqn{i} is considered as
    lethal, if the deletion of this gene results in a zero flux rate in the
    objective function given in \code{model}.
    Default: \code{TRUE}.
  }
  \item{tol}{
    A single numeric value, containing an absolute threshold value for a gene
    being lethal or not.\cr
    Default: \code{SYBIL_SETTINGS("TOLERANCE")}.
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
  The function \code{doubleGeneDel} studies the effect of genetic perturbations
  by double gene deletions on the phenotype of the metabolic network. The
  function performs \eqn{n} optimizations with \eqn{n} being either the length
  of the character vector in argument \code{geneList1} times the length of the
  character vector in argument \code{geneList2}, if argument \code{allComb} is
  set to \code{TRUE}, or the length of one of these vectors if argument
  \code{allComb} is set to \code{FALSE}. For each gene deletion \eqn{i,j}
  the set of fluxes effected by the simultaneous deletion of genes \eqn{i} and
  \eqn{j} is constrained to zero flux. If the deletion of a certain pair of
  genes has an effect, is tested with the function \code{\link{geneDel}}. Each
  optimization corresponds to the simultaneous deletion of two genes.
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
\dontrun{
  ## compute all possible pairwise gene deletions
  # load example data set
  data(Ec_core)
  
  # compute all possible pairwise gene deletions via
  # FBA (default)
  Ec_dgd <- doubleGeneDel(Ec_core, allComb = TRUE)
  
  # or MOMA (linearized version)
  Ec_dgd <- doubleGeneDel(Ec_core,
                          allComb = TRUE,
                          algorithm = "lmoma")
}
}

\keyword{optimize}

