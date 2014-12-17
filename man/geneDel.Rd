\name{geneDel}
\alias{geneDel}

\encoding{utf8}

\title{
  Get Gene-Reaction Association
}

\description{
  The function \code{geneDel} returns the fluxes which are effected by a
  particular combination of genes.
}

\usage{
  geneDel(model, genes, checkId = FALSE)
}

\arguments{
  \item{model}{
    An object of class \code{modelorg}.
  }
  \item{genes}{
    A vector of character strings of gene id's used in \code{model}, or an
    integer vector with indices to gene id's in \code{\link{allGenes}(model)}.
  }
  \item{checkId}{
    Boolean. If set to \code{TRUE}, argument \code{genes} will be checked wether
    it fits to \code{model} (e.g. are all genes existing). If set to
    \code{FALSE}, \code{genes} must contain indices of gene id's in
    \code{model}, e.g. in calls from \code{\link{optimizer}}.
  }
}

\details{
  The function \code{geneDel} checks for a set of gene id's in \code{gene}
  on which fluxes a deletion of this set of genes has an effect.
}

\value{
  An numeric vector of pointers to reaction id's in \code{model} or \code{NULL},
  if no fluxes are effected by the gene deletion.
}

\references{
  Edwards, J. S., Ibarra, R. U. and Palsson, B. Ø. (2001) In silico predictions
  of \emph{Escherichia coli} metabolic capabilities are consistent with
  experimental data. \emph{Nat Biotechnol} \bold{19}, 125--130.
}

\author{
  Gabriel Gelius-Dietrich
}

\seealso{
  \code{\link{optimizer}}
}

