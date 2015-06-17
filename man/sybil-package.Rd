\name{sybil-package}

\alias{sybil-package}
\alias{sybil}

\encoding{utf8}

\docType{package}

\title{
  sybil -- Efficient Constrained Based Modelling in R
}

\description{
  The package \pkg{sybil} is a collection of functions designed for
  in silico analysis---in particular constrained based analysis---of metabolic
  networks.
}

\details{

  The package sybil is designed to read metabolic networks from csv files.
  This is done by the function \code{\link{readTSVmod}}. The function returns
  an object of the class \code{\linkS4class{modelorg}}.

  Read csv files (example files included):
  \preformatted{
mpath <- system.file(package = "sybil", "extdata")
model <- readTSVmod(prefix = "Ec_core",
                    fpath = mpath, quote = "\"")
  }
  Perform flux balance analysis (FBA):\cr
  \code{ec_f <- optimizeProb(model)}

  Perform single gene deletion analysis:\cr
  \code{ec_g <- oneGeneDel(model)}

  Plot the values of the objective function after optimization in a
  histogram:\cr
  \code{plot(ec_g)}

  Perform flux variability analysis:\cr
  \code{ec_v <- fluxVar(model)}

  Plot the result:\cr
  \code{plot(ec_v)}
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\references{
  The BiGG database \url{http://bigg.ucsd.edu/}.

  Schellenberger, J., Park, J. O., Conrad, T. C., and Palsson, B. Ø., (2010)
  BiGG: a Biochemical Genetic and Genomic knowledgebase of large scale metabolic
  reconstructions. \emph{BMC Bioinformatics} \bold{11}, 213.

  The openCOBRA project \url{http://opencobra.sourceforge.net/}.

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

\seealso{
  Package \pkg{sybilSBML} and there the function \code{readSBMLmod} to read
  metabolic models written in SBML language.
}

\examples{
  data(Ec_core)
  Ec_ofd <- oneGeneDel(Ec_core)
  plot(Ec_ofd)
}

\keyword{package}

