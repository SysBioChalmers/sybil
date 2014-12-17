\name{mod2irrev}
\alias{mod2irrev}

\encoding{utf8}

\title{Produces a Model in Irreversible Format}

\description{
  The function \code{mod2irrev} produces a model with all reactions
  moving in positive direction.
}

\usage{
  mod2irrev(model, exex = FALSE)
}

\arguments{
  \item{model}{An object of class \code{\linkS4class{modelorg}}.}
  \item{exex}{Boolean. Exclude exchange fluxes (default: FALSE).}
}

\details{
  The returned model consists only of reactions moving in positive
  direction. Reactions with a negative direction in the original model
  are transferred to positive direction; the corresponding reaction id
  gets extended by \dQuote{\code{_r}}.

  Reversible reactions are split into two reactions. The corresponding
  reaction ids gets extended by \dQuote{\code{_f}}, or \dQuote{\code{_b}}
  indicating the original direction.

  If \code{exex = TRUE}, the exchange reactions were obtained by
  \code{findExchReact}.
}

\value{
  An object of class \code{\link{modelorg_irrev}}.
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

\author{Gabriel Gelius-Dietrich, Markus Herrgard}

\seealso{\code{\linkS4class{modelorg_irrev}}}

\examples{
  data(Ec_core)
  Ec_ir <- mod2irrev(Ec_core)
}

\keyword{manip}


