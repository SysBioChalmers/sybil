\name{phpp}
\alias{phpp}

\encoding{utf8}

\title{
  Phenotypic Phase Plane Analysis
}

\description{
  Performs phenotypic phase plane analysis for a given metabolic model.
}

\usage{
phpp(model, ctrlreact, rng = c(0, 0, 20, 20),
     numP = 50, setToZero = TRUE, redCosts = FALSE, ...)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{ctrlreact}{
    An object of class \code{\linkS4class{reactId}}, character or integer.
    Specifies two control reactions.
  }
  \item{rng}{
    A numeric vector of length four, giving the lower and upper bounds of the
    control reactions. The first two values contain the lower bounds, the last
    two values the upper bounds.\cr
    Default: \code{c(0, 0, 20, 20)}
  }
  \item{numP}{
    The number of points to analyse.\cr
    Default: \code{50}
  }
  \item{setToZero}{
    Logical: If the mathematical programming software returns a solution status
    which is not optimal, set the corresponding objective value to zero (see
    also \code{\link{optimizer}}).\cr
    Default: \code{TRUE}.
  }
  \item{redCosts}{
    Logical: store reduced costs of the control variables.\cr
    Default: \code{FALSE}.
  }
  \item{\dots}{
    Further arguments passed to \code{\link{optimizer}}.
  }
}

\details{
  The two control reactions given in argument \code{ctrlreact} are treated as
  uptake reactions: reactions that transport metabolites into the metabolic
  network. That means, the optimizations are performed using
  \code{abs(rng) * -1}.
}

\value{
  An object of class \code{\linkS4class{optsol_phpp}}.
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

  Edwards, J. S., Ibarra, R. U. and Palsson, B. Ø. (2001) In silico predictions
  of \emph{Escherichia coli} metabolic capabilities are consistent with
  experimental data.
  \emph{Nat Biotechnol} \bold{19}, 125--130.

  Edwards, J. S., Ramakrishna, R. and Palsson, B. Ø. (2002) Characterizing the
  metabolic phenotype: a phenotype phase plane analysis.
  \emph{Biotechnol Bioeng} \bold{77}, 27--36.

  Bernhard Ø. Palsson (2006).
  \emph{Systems Biology: Properties of Reconstructed Networks}.
  Cambridge University Press.
}

\author{Gabriel Gelius-Dietrich}

\examples{
  data(Ec_core)
  
  # switch off glucose input
  Ec_core_wo_glc <- changeUptake(Ec_core, off = "glc_D[e]")
  opt <- phpp(Ec_core_wo_glc, ctrlreact = c("EX_succ(e)", "EX_o2(e)"))
  
  # plot phenotypic phase plane
  plot(opt)
  
  # plot reduced costs of the two control reactions
  plot(opt, "EX_succ(e)")
  plot(opt, "EX_o2(e)")
}

\keyword{optimize}

