\name{fluxVar}
\alias{fluxVar}

\encoding{utf8}

\title{
  Flux Variability Analysis
}
\description{
  Performs flux variability analysis for a given model.
}
\usage{
  fluxVar(model, react = c(1:react_num(model)), exex = FALSE, ...)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{react}{
    An object of class \code{\linkS4class{reactId}}, character or integer.
    Specifies the fluxes (variables) to analyse.\cr
    Default: all reactions present in \code{model}.
  }
  \item{exex}{
    Boolean.  Exclude exchange reactions from analysis. If set to \code{TRUE},
    argument \code{react} will be ignored. All reactions present in \code{model}
    will be used, except for the exchange reactions.\cr
    Default: \code{FALSE}
  }
  \item{\dots}{
    Further arguments passed to \code{\link{optimizer}}. Argument
    \code{algorithm} is set to \code{"\link[=sysBiolAlg_fv-class]{fv}"}, further
    possible arguments are \code{fld}, arguments for pre and post processing
    commands, \code{verboseMode} and further arguments passed to the constructor
    for objects of class \code{\linkS4class{sysBiolAlg_fv}}, see there for
    details.
  }
}

\details{
  The algorithm is described in
  \code{\linkS4class{sysBiolAlg_fv}}.
}

\value{
  An object of class \code{\linkS4class{optsol_fluxVar}}. The first \eqn{1} to
  \eqn{n} (with \eqn{n} being the number of elements in argument \code{react})
  solutions are from the minimizations, and the last \eqn{n+1} to \eqn{2n}
  solutions are from the maximizations.
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

  Bernhard Ø. Palsson (2006).
  \emph{Systems Biology: Properties of Reconstructed Networks}.
  Cambridge University Press.
}

\author{Gabriel Gelius-Dietrich}

\examples{
  data(Ec_core)
  fv <- fluxVar(Ec_core)
  plot(fv)
}

\keyword{optimize}

