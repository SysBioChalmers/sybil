\name{geneDeletion}
\alias{geneDeletion}

\encoding{utf8}

\title{
  Gene Deletion Experiments
}

\description{
  The function \code{geneDeletion} studies the effect of \eqn{n} in silico gene
  deletions on the phenotype of a metabolic network. The value of \eqn{n} is the
  number of genes knocked-out simultaneously.
}

\usage{
  geneDeletion(model, genes, combinations = 1,
               lb = NULL, ub = NULL, checkOptSolObj = FALSE, ...)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{genes}{
    Character or Integer: the genes to delete (see Details below).
  }
  \item{combinations}{
    A single integer value. If \code{combinations > 1} and \code{genes} is not a
    matrix, \code{combinations} is the number of elements from \code{genes}
    taken at a time while building all combinations of the elements in
    \code{genes} (see Details below).\cr
    Default: \code{1}.
  }
  \item{lb}{
    A numeric vector containing the lower bounds for the reaction rates of
    reactions (variables) affected by the genes given in argument
    \code{genes}. If set to \code{NULL}, all reactions affected will be
    constrained to zero.\cr
    Default: \code{NULL}.
  }
  \item{ub}{
    A numeric vector containing the upper bounds for the reaction rates of
    reactions (variables) affected by the genes given in argument
    \code{genes}. If set to \code{NULL}, all reactions affected will be
    constrained to zero.\cr
    Default: \code{NULL}.
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
  If argument \code{genes} is a matrix of character values (gene id's) or
  integers (pointers to gene id's), each column is treated as one deletion
  experiment. If the matrix is made up of integers, a zero entry means no gene.
  
  If argument \code{genes} is a character vector or integer, the argument
  \code{combinations} gives the number of gene id's taken each time in order
  to build all possible combinations of \code{genes}. A matrix is constructed
  using \code{\link{combn}}. The value of argument \code{combinations} gives the
  number of genes, which are knocked-out simultaneously. The default value
  \eqn{1} performs a single gene deletion experiment, like the function
  \code{\link{oneGeneDel}} does. A value of \eqn{2} performs a double gene
  deletion as described in \code{\link{doubleGeneDel}}. A value of \eqn{n}
  performs an \eqn{n} gene deletion experiment. Keep in mind, that the number
  of optimizations will get very high for increasing values of
  \code{combinations}.
  
  If argument \code{genes} is empty, the number of unique genes present in
  \code{model} is used.

  The required length of arguments \code{lb} and \code{ub} (if not \code{NULL})
  depends on the values given in arguments \code{genes} and \code{combinations}.
  If \code{genes} is a matrix, \code{lb} and \code{ub} must be of length equal
  to the number of columns in \code{genes}. If genes is a vector, \code{lb} and
  \code{ub} must be of length equal to \code{length(genes) * combinations}.
}

\value{
  An object of class \code{\linkS4class{optsol_genedel}}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\linkS4class{modelorg}}, \code{\linkS4class{optsol}},
  \code{\linkS4class{optsol_genedel}},
  \code{\link{checkOptSol}}, \code{\link{oneGeneDel}},
  \code{\link{optimizer}}, \code{\link{optimizeProb}},
  \code{\link{combn}} and \code{\link{SYBIL_SETTINGS}}.
}

\examples{
  ## load the dataset
  data(Ec_core)
  
  ## perform a single gene deletion analysis
  ## (delete every gene one by one) via FBA
  gd <- geneDeletion(Ec_core)
  
  ## or via MOMA (linearized version)
  gd <- geneDeletion(Ec_core, algorithm = "lmoma")
  
  ## triple gene deletion analysis using the first ten genes
  gd <- geneDeletion(Ec_core, genes = 10, combinations = 3)

\dontrun{
  ## perform a double gene deletion analysis
  ##(delete all possible pairwise combinations of all genes)
  gd <- geneDeletion(Ec_core, combinations = 2)

  ## perform a triple gene deletion analysis
  ## (very high number of optimizations)
  gd <- geneDeletion(Ec_core, combinations = 3)
}
}

\keyword{optimize}

