\name{modelorg2ExPA}
\alias{modelorg2ExPA}

\encoding{utf8}

\title{
  Write an Instance of Class modelorg to File in ExPA Format
}

\description{
  The function \code{modelorg2ExPA} writes the content of an instance of class
  \code{\link{modelorg}} to text files in a format which can be read by the
  program ExPA to compute extreme pathways.
}

\usage{
  modelorg2ExPA(model, fname = NULL, exIntReact = NULL,
                filepath = ".", suffix = "expa",
                tol = SYBIL_SETTINGS("TOLERANCE"))
}

\arguments{
  \item{model}{
    An object of class \code{\link{modelorg}}.
  }
  \item{fname}{
    An single character string giving the filename to write to.\cr
    Default: \code{<model_id>.expa}.
  }
  \item{exIntReact}{
    An object of class \code{\linkS4class{reactId}}, character or integer,
    giving id's of internal reactions to exclude in the ExPA file.\cr
    Default: \code{NULL}.
  }
  \item{filepath}{
    A single character string giving the path to a certain directory in which
    the output files will be stored.\cr
    Default: \code{"."}.
  }
  \item{suffix}{
    A single character string giving the file name extension.\cr
    Default: \code{"expa"}.
  }
  \item{tol}{
    A single numeric value giving the limit of tolerance. An element
    \eqn{S_{ij}}{S[i,j]} of the stoichiometric matrix is treated as non-zero,
    if \eqn{|S_{ij}| > \textrm{tol}}{S[i,j] > tol} is true.\cr
    Default: \code{"expa"}.
  }
}

\details{
  The function \code{modelorg2ExPA} produces input files for the program ExPA.
  With ExPA, it is possible to calculate extreme pathways in metabolic networks.
  
  The function produces a warning, if a reaction contains non-integer
  stoichiometric values, because they are not compatible with the ExPA program.
}

\value{
  Returns \code{TRUE} invisibly on success.
}

\references{
  Bell, S. L. and Palsson, B. Ø. (2005) Expa: a program for calculating extreme
  pathways in biochemical reaction networks. \emph{Bioinformatics} \bold{21},
  1739--1740.

  The ExPA homepage \url{http://gcrg.ucsd.edu/Downloads/ExtremePathwayAnalysis}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\keyword{ IO }
